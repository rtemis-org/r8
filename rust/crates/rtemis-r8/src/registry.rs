//! The declarations of a session: types, traits, and impls by namespace,
//! the namespaces' owners, and the saved states that a failed package load
//! returns to.
//!
//! One lock holds the whole state, so each operation is atomic across the
//! documents, the owners, and the saved states. Readers take a snapshot, an
//! `Arc` of the documents; no guard leaves the registry, so no host holds
//! the lock across a call back into the host. A change replaces the
//! documents by copy on write (`Arc::make_mut`), which copies nothing while
//! no snapshot is alive.
//! spec: R8/class-system#registry

use std::collections::HashMap;
use std::sync::{Arc, Mutex, MutexGuard, PoisonError};

use crate::ir::{
    DeclError, ImplDef, TraitDef, TypeDef, check_defaults, check_document_records, check_extends,
    check_tags, check_trait, check_type,
};
use crate::name::{Name, QualifiedName};
use crate::record::{self, Declaration, DeclarationRef, Entry};
use crate::rules::{DocumentSet, TypeLookup};

/// Who owns a namespace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Owner {
    /// The package that first declared into the namespace.
    Package(String),
    /// Code outside a package declared into the namespace; a package may
    /// claim it.
    Unowned,
}

impl Owner {
    fn of(package: &str) -> Owner {
        if package.is_empty() {
            Owner::Unowned
        } else {
            Owner::Package(package.to_string())
        }
    }
}

/// What a change did.
#[derive(Debug, Default)]
pub struct Outcome {
    /// The documents differ after the change.
    pub changed: bool,
    /// The declaration's problems, with paths relative to the declaration;
    /// a declaration with problems leaves the documents unchanged.
    pub problems: Vec<DeclError>,
    /// The structs that the declaration leaves stale: the declared struct,
    /// when its parent is stale, and its descendants that disagree with
    /// their parents.
    pub stale: Vec<QualifiedName>,
}

/// A problem of the registry itself.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegistryError {
    /// A declaration's check panicked; the registry is unchanged.
    CheckPanicked,
    /// `restore()` or `release()` was given a token other than the most
    /// recent `save()`'s.
    Unbalanced,
}

impl std::fmt::Display for RegistryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegistryError::CheckPanicked => {
                f.write_str("the declaration's check failed; the registry is unchanged")
            }
            RegistryError::Unbalanced => f.write_str(
                "the registry's saved states are restored or released in the reverse order of their saves",
            ),
        }
    }
}

/// A saved state, which `restore()` brings back and `release()` discards;
/// a host passes its number across its own interface.
#[derive(Debug, PartialEq, Eq)]
#[must_use = "a saved state is restored or released"]
pub struct SaveToken(pub u64);

struct Saved {
    token: u64,
    documents: Arc<DocumentSet>,
    owners: HashMap<String, Owner>,
}

#[derive(Default)]
struct State {
    documents: Arc<DocumentSet>,
    owners: HashMap<String, Owner>,
    saved: Vec<Saved>,
    generation: u64,
    next_token: u64,
}

impl State {
    fn changed(&mut self) {
        self.generation += 1;
    }

    fn put_type(&mut self, ns: &Name, def: TypeDef) -> Result<Outcome, RegistryError> {
        let q = QualifiedName {
            namespace: ns.clone(),
            name: def.name().clone(),
        };
        if TypeLookup::find(&*self.documents, &q) == Some(&def) {
            return Ok(Outcome::default());
        }
        let docs = Arc::make_mut(&mut self.documents);
        let placed = docs.put_type(ns, def);
        let (d, index) = (placed.document, placed.index);
        let checked = guarded(|| {
            let mut p = check_type(docs.document(d), index);
            if p.is_empty()
                && let TypeDef::Struct(s) = &docs.document(d).types[index]
            {
                let at = format!("/types/{index}");
                p = check_extends(s, &q, &at, &*docs, false);
                if p.is_empty() {
                    p = check_defaults(s, &q, &at, &*docs);
                }
                if p.is_empty() && s.extends.is_some() {
                    p = inherited_tag_problems(&*docs, &q);
                }
            }
            p
        });
        match checked {
            Ok(p) if p.is_empty() => {
                let stale = refresh_stale(docs, &q);
                self.changed();
                Ok(Outcome {
                    changed: true,
                    problems: p,
                    stale,
                })
            }
            Ok(p) => {
                docs.undo_put(placed);
                Ok(Outcome {
                    changed: false,
                    problems: relative(p, &format!("/types/{index}")),
                    stale: Vec::new(),
                })
            }
            Err(e) => {
                docs.undo_put(placed);
                Err(e)
            }
        }
    }

    fn put_trait(&mut self, ns: &Name, def: TraitDef) -> Result<Outcome, RegistryError> {
        if self
            .documents
            .namespace(ns)
            .is_some_and(|doc| doc.traits.contains(&def))
        {
            return Ok(Outcome::default());
        }
        let docs = Arc::make_mut(&mut self.documents);
        let placed = docs.put_trait(ns, def);
        let (d, index) = (placed.document, placed.index);
        let checked = guarded(|| check_trait(docs.document(d), index));
        match checked {
            Ok(p) if p.is_empty() => {
                self.changed();
                Ok(Outcome {
                    changed: true,
                    problems: p,
                    stale: Vec::new(),
                })
            }
            Ok(p) => {
                docs.undo_put_trait(placed);
                Ok(Outcome {
                    changed: false,
                    problems: relative(p, &format!("/traits/{index}")),
                    stale: Vec::new(),
                })
            }
            Err(e) => {
                docs.undo_put_trait(placed);
                Err(e)
            }
        }
    }

    /// Puts an impl; an impl of the same trait for the same target, with the
    /// same type arguments, is replaced.
    /// Puts an impl and checks the tag values of its trait's implementors
    /// (`check_tags()`); an impl with problems is undone.
    fn put_impl_checked(&mut self, ns: &Name, def: ImplDef) -> Result<Outcome, RegistryError> {
        if self
            .documents
            .namespace(ns)
            .is_some_and(|doc| doc.impls.contains(&def))
        {
            return Ok(Outcome::default());
        }
        let tr = def.r#trait.clone();
        let docs = Arc::make_mut(&mut self.documents);
        let d = docs.document_position(ns);
        let impls = docs.impls_mut(d);
        let mut removed = Vec::new();
        impls.retain(|i| {
            let same =
                i.r#trait == def.r#trait && i.type_args == def.type_args && i.target == def.target;
            if same {
                removed.push(i.clone());
            }
            !same
        });
        impls.push(def);
        let checked = guarded(|| check_tags(&*docs, &tr, &|_| String::new()));
        match checked {
            Ok(p) if p.is_empty() => {
                self.changed();
                Ok(Outcome {
                    changed: true,
                    problems: p,
                    stale: Vec::new(),
                })
            }
            other => {
                let impls = docs.impls_mut(d);
                impls.pop();
                impls.extend(removed);
                docs.remove_document_if_empty(d);
                other.map(|p| Outcome {
                    changed: false,
                    problems: p,
                    stale: Vec::new(),
                })
            }
        }
    }

    fn put_impl(&mut self, ns: &Name, def: ImplDef) -> bool {
        if self
            .documents
            .namespace(ns)
            .is_some_and(|doc| doc.impls.contains(&def))
        {
            return false;
        }
        let docs = Arc::make_mut(&mut self.documents);
        let d = docs.document_position(ns);
        let impls = docs.impls_mut(d);
        impls.retain(|i| {
            !(i.r#trait == def.r#trait && i.type_args == def.type_args && i.target == def.target)
        });
        impls.push(def);
        self.changed();
        true
    }

    /// Puts a declaration without its check, as a package's records at load
    /// are put before `check_namespaces()` checks them whole.
    fn put_unchecked(&mut self, ns: &Name, d: Declaration) -> bool {
        match d {
            Declaration::Type(t) => {
                let q = QualifiedName {
                    namespace: ns.clone(),
                    name: t.name().clone(),
                };
                if TypeLookup::find(&*self.documents, &q) == Some(&t) {
                    return false;
                }
                let docs = Arc::make_mut(&mut self.documents);
                let _ = docs.put_type(ns, t);
                refresh_stale(docs, &q);
                self.changed();
                true
            }
            Declaration::Trait(t) => {
                if self
                    .documents
                    .namespace(ns)
                    .is_some_and(|doc| doc.traits.contains(&t))
                {
                    return false;
                }
                let _ = Arc::make_mut(&mut self.documents).put_trait(ns, t);
                self.changed();
                true
            }
            Declaration::Impl(i) => self.put_impl(ns, i),
        }
    }
}

/// The tag problems of the traits that the ancestors of the struct `q`
/// implement, which `q` inherits: its tag value and its fields enter their
/// checks.
fn inherited_tag_problems(docs: &DocumentSet, q: &QualifiedName) -> Vec<DeclError> {
    let ancestors = docs.ancestors(q);
    let mut traits: Vec<&QualifiedName> = Vec::new();
    for doc in docs.documents() {
        for i in &doc.impls {
            if let crate::ir::ImplTarget::Type { name } = &i.target
                && ancestors.contains(name)
                && !traits.contains(&&i.r#trait)
            {
                traits.push(&i.r#trait);
            }
        }
    }
    traits
        .into_iter()
        .flat_map(|t| check_tags(docs, t, &|_| String::new()))
        .collect()
}

/// Marks the struct `q` and its descendants stale or not, every parent
/// before its children: a struct is stale when its parent is stale or its
/// inherited fields or rules disagree with its parent's. Returns the stale
/// ones.
fn refresh_stale(docs: &mut DocumentSet, q: &QualifiedName) -> Vec<QualifiedName> {
    // A struct outside every chain, when nothing is stale, stays so.
    let in_chain = matches!(
        TypeLookup::find(&*docs, q),
        Some(TypeDef::Struct(s)) if s.extends.is_some()
    );
    if !in_chain && docs.children(q).is_empty() && !docs.has_stale() {
        return Vec::new();
    }
    let mut order = vec![q.clone()];
    order.extend(docs.descendants(q));
    let mut out = Vec::new();
    for t in &order {
        let stale = match TypeLookup::find(&*docs, t) {
            Some(TypeDef::Struct(s)) => s.extends.as_ref().is_some_and(|p| {
                docs.is_stale(p) || !check_extends(s, t, "", &*docs, true).is_empty()
            }),
            _ => false,
        };
        docs.set_stale(t, stale);
        if stale {
            out.push(t.clone());
        }
    }
    out
}

/// Paths of a document's problems, relative to the declaration at `prefix`.
fn relative(problems: Vec<DeclError>, prefix: &str) -> Vec<DeclError> {
    problems
        .into_iter()
        .map(|mut e| {
            if let Some(rest) = e.path.strip_prefix(prefix) {
                e.path = rest.to_string();
            }
            e
        })
        .collect()
}

/// Runs a declaration's check; a panic of the check is an error, after
/// which the caller undoes the put.
fn guarded(check: impl FnOnce() -> Vec<DeclError>) -> Result<Vec<DeclError>, RegistryError> {
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(check))
        .map_err(|_| RegistryError::CheckPanicked)
}

/// One problem of a batch of declarations, with the qualified name of the
/// declaration it belongs to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BatchProblem {
    /// The qualified name of the type or trait.
    pub name: String,
    /// The problem, with its path relative to the declaration.
    pub problem: DeclError,
}

/// The declarations of a session.
#[derive(Default)]
pub struct Registry {
    state: Mutex<State>,
}

impl Registry {
    /// An empty registry.
    pub fn new() -> Registry {
        Registry::default()
    }

    /// The state; a lock poisoned by a panic elsewhere is recovered, since
    /// every change leaves the state whole before it can panic.
    fn state(&self) -> MutexGuard<'_, State> {
        self.state.lock().unwrap_or_else(PoisonError::into_inner)
    }

    /// The documents at the call; later changes do not reach the snapshot.
    pub fn snapshot(&self) -> Arc<DocumentSet> {
        Arc::clone(&self.state().documents)
    }

    /// A number that increases with each change of the documents.
    pub fn generation(&self) -> u64 {
        self.state().generation
    }

    /// Checks the type declaration `def` in the namespace `ns` and registers
    /// it; a declaration with problems leaves the registry unchanged.
    pub fn declare_type(&self, ns: &Name, def: TypeDef) -> Result<Outcome, RegistryError> {
        self.state().put_type(ns, def)
    }

    /// Checks the trait declaration `def` and registers it, replacing a
    /// trait of the same name.
    pub fn declare_trait(&self, ns: &Name, def: TraitDef) -> Result<Outcome, RegistryError> {
        self.state().put_trait(ns, def)
    }

    /// Registers an impl, replacing one of the same trait for the same
    /// target; whether the documents changed.
    pub fn declare_impl(&self, ns: &Name, def: ImplDef) -> Result<Outcome, RegistryError> {
        self.state().put_impl_checked(ns, def)
    }

    /// Declares the entries of declaration records in order, under one
    /// lock: each through its check, or, with `check` false, without it, as
    /// at a package's load, after which `check_namespaces()` checks the
    /// package's namespaces whole. Returns whether the documents changed,
    /// and the problems.
    pub fn declare_records(
        &self,
        entries: Vec<Entry>,
        check: bool,
    ) -> Result<(bool, Vec<BatchProblem>), RegistryError> {
        let mut state = self.state();
        let mut changed = false;
        let mut problems = Vec::new();
        for entry in entries {
            let ns = entry.namespace;
            if !check {
                changed |= state.put_unchecked(&ns, entry.declaration);
                continue;
            }
            let (name, outcome) = match entry.declaration {
                Declaration::Type(t) => {
                    let name = format!("{ns}::{}", t.name());
                    (name, state.put_type(&ns, t)?)
                }
                Declaration::Trait(t) => {
                    let name = format!("{ns}::{}", t.name);
                    (name, state.put_trait(&ns, t)?)
                }
                Declaration::Impl(i) => {
                    let target = match &i.target {
                        crate::ir::ImplTarget::Type { name } => name.to_string(),
                        crate::ir::ImplTarget::HostClass { class, .. } => class.clone(),
                    };
                    let name = format!("impl {} for {target}", i.r#trait);
                    (name, state.put_impl_checked(&ns, i)?)
                }
            };
            changed |= outcome.changed;
            problems.extend(outcome.problems.into_iter().map(|problem| BatchProblem {
                name: name.clone(),
                problem,
            }));
        }
        Ok((changed, problems))
    }

    /// The problems of whole namespaces, each document checked once: what a
    /// declaration's own check cannot see, such as a rule that reads a field
    /// of a type declared again without that field.
    ///
    /// A struct of these namespaces whose parent is in another namespace is
    /// checked against it, so that a package built against an earlier
    /// declaration of a parent reports `extends_stale`. A struct of another
    /// namespace that the parent's declaration leaves stale is not a problem
    /// of these namespaces; [`Registry::stale_types`] lists it.
    pub fn check_namespaces(&self, namespaces: &[String]) -> Vec<(String, DeclError)> {
        let docs = self.snapshot();
        let mut out = Vec::new();
        for n in namespaces {
            if let Some(doc) = docs.documents().iter().find(|d| d.namespace.as_str() == n) {
                out.extend(
                    check_document_records(doc)
                        .into_iter()
                        .map(|e| (n.clone(), e)),
                );
            }
        }
        // The tag values of every trait declared or implemented in these
        // namespaces.
        let mut traits: Vec<(String, QualifiedName)> = Vec::new();
        for doc in docs.documents() {
            if !namespaces.iter().any(|n| n == doc.namespace.as_str()) {
                continue;
            }
            let declared = doc.traits.iter().map(|t| QualifiedName {
                namespace: doc.namespace.clone(),
                name: t.name.clone(),
            });
            let implemented = doc.impls.iter().map(|i| i.r#trait.clone());
            for t in declared.chain(implemented) {
                if !traits.iter().any(|(_, x)| *x == t) {
                    traits.push((doc.namespace.to_string(), t));
                }
            }
        }
        for (ns, t) in &traits {
            out.extend(
                check_tags(&*docs, t, &|_| String::new())
                    .into_iter()
                    .map(|e| (ns.clone(), e)),
            );
        }
        for doc in docs.documents() {
            if !namespaces.iter().any(|n| n == doc.namespace.as_str()) {
                continue;
            }
            for (i, t) in doc.types.iter().enumerate() {
                let TypeDef::Struct(s) = t else { continue };
                let Some(parent) = &s.extends else { continue };
                // `check_document()` checks a parent in the child's own
                // document.
                if parent.namespace == doc.namespace {
                    continue;
                }
                let q = QualifiedName {
                    namespace: doc.namespace.clone(),
                    name: s.name.clone(),
                };
                out.extend(
                    check_extends(s, &q, &format!("/types/{i}"), &*docs, true)
                        .into_iter()
                        .map(|e| (doc.namespace.to_string(), e)),
                );
            }
        }
        out
    }

    /// Claims `namespace` for `package` (`""` for code outside a package):
    /// the first package to declare into a namespace owns it, and code
    /// outside a package owns none. Returns the package that owns it already
    /// and refuses the claim, if any.
    pub fn claim(&self, namespace: &str, package: &str) -> Option<String> {
        let mut state = self.state();
        match state.owners.get(namespace) {
            Some(Owner::Package(owner)) if owner != package => Some(owner.clone()),
            Some(Owner::Package(_)) => None,
            Some(Owner::Unowned) if package.is_empty() => None,
            _ => {
                state
                    .owners
                    .insert(namespace.to_string(), Owner::of(package));
                None
            }
        }
    }

    /// The stale structs, sorted by qualified name.
    pub fn stale_types(&self) -> Vec<QualifiedName> {
        self.snapshot().stale_types()
    }

    /// The package that owns `namespace`, without claiming it.
    pub fn owner(&self, namespace: &str) -> Option<String> {
        match self.state().owners.get(namespace) {
            Some(Owner::Package(p)) => Some(p.clone()),
            _ => None,
        }
    }

    /// The namespaces `package` owns, sorted.
    pub fn namespaces_of(&self, package: &str) -> Vec<String> {
        let state = self.state();
        let mut out: Vec<String> = state
            .owners
            .iter()
            .filter(|(_, o)| matches!(o, Owner::Package(p) if p == package))
            .map(|(ns, _)| ns.clone())
            .collect();
        out.sort();
        out
    }

    /// The declaration record of the registered type `q` and of every
    /// named type it reaches, the reached types first; `None` when `q` is
    /// not registered.
    pub fn type_record(&self, q: &QualifiedName) -> Option<Vec<u8>> {
        let docs = self.snapshot();
        let names = docs.reached(q);
        if names.is_empty() {
            return None;
        }
        Some(record::encode(names.iter().filter_map(|n| {
            TypeLookup::find(&*docs, n).map(|d| (&n.namespace, DeclarationRef::Type(d)))
        })))
    }

    /// Saves the documents and the owners, which `restore()` brings back and
    /// `release()` discards; saves nest.
    pub fn save(&self) -> SaveToken {
        let mut state = self.state();
        state.next_token += 1;
        let token = state.next_token;
        let saved = Saved {
            token,
            documents: Arc::clone(&state.documents),
            owners: state.owners.clone(),
        };
        state.saved.push(saved);
        SaveToken(token)
    }

    /// Restores the state the most recent save holds, which `token` names.
    pub fn restore(&self, token: SaveToken) -> Result<(), RegistryError> {
        let mut state = self.state();
        match state.saved.last() {
            Some(s) if s.token == token.0 => {}
            _ => return Err(RegistryError::Unbalanced),
        }
        let s = state.saved.pop().expect("checked above");
        let changed = !Arc::ptr_eq(&s.documents, &state.documents);
        state.documents = s.documents;
        state.owners = s.owners;
        if changed {
            state.changed();
        }
        Ok(())
    }

    /// Discards the state the most recent save holds, which `token` names.
    pub fn release(&self, token: SaveToken) -> Result<(), RegistryError> {
        let mut state = self.state();
        match state.saved.last() {
            Some(s) if s.token == token.0 => {
                state.saved.pop();
                Ok(())
            }
            _ => Err(RegistryError::Unbalanced),
        }
    }
}
