//! The signatures of the methods of `#[r8::methods]`: the R8 types of their
//! parameters and results, which registration checks against the trait's
//! declaration, and [`TraitDecl`], the declaration of a trait in Rust.
//! spec: R8/class-system#signatures-of-impls-in-rust

use rtemis_r8::derive::{Constraints, Encode, constrain};
use rtemis_r8::ir::{
    FieldType, MethodDef, Param, ParamDef, RequiredField, ReturnType, SelfMarker, TraitDef,
};
use rtemis_r8::name::{MethodName, Name};
use rtemis_r8::rules::DocumentSet;
use rtemis_r8::tree::Tree;

/// The R8 type of a Rust parameter type.
#[derive(Debug, Clone, PartialEq)]
pub enum ParamType {
    /// A field type, which R8 checks at a call.
    Typed(FieldType),
    /// A value of R, of any type.
    Untyped,
    /// `...`.
    Dots,
}

impl ParamType {
    /// `option()` of a type.
    pub(crate) fn optional(self) -> Self {
        match self {
            ParamType::Typed(t) => ParamType::Typed(FieldType::Option { inner: Box::new(t) }),
            other => other,
        }
    }
}

/// What the attributes of a parameter state beside its Rust type.
#[doc(hidden)]
#[derive(Default)]
pub struct Facts {
    pub constraints: Constraints,
    pub default: Option<Tree>,
    pub description: &'static str,
}

/// A parameter of a method.
///
/// # Panics
///
/// When a constraint does not apply to the parameter's type, or a parameter
/// without a type has one.
#[doc(hidden)]
pub fn param(name: &str, ty: ParamType, facts: Facts) -> ParamDef {
    let (name, ty) = match ty {
        ParamType::Dots => ("...", None),
        ParamType::Untyped => (name, None),
        ParamType::Typed(t) => (
            name,
            Some(
                constrain(t, facts.constraints, false)
                    .unwrap_or_else(|e| panic!("parameter `{name}`: {e}")),
            ),
        ),
    };
    if ty.is_none() && (facts.default.is_some() || facts.constraints != Constraints::default()) {
        panic!("parameter `{name}`: a parameter without a type has no constraints and no default");
    }
    Param {
        name: name.to_string(),
        dispatch: false,
        ty,
        default: facts.default.map(Box::new),
        host_default: None,
        description: facts.description.into(),
    }
    .into()
}

/// A parameter that the method dispatches on; `marked` in a method that
/// dispatches on several.
#[doc(hidden)]
pub fn dispatched(name: &str, marked: bool, description: &str) -> ParamDef {
    Param {
        name: name.to_string(),
        dispatch: marked,
        ty: None,
        default: None,
        host_default: None,
        description: description.into(),
    }
    .into()
}

/// The result of a method, of a type that R8 knows, or of any.
#[doc(hidden)]
pub fn returns(ty: Option<FieldType>) -> Option<ReturnType> {
    ty.map(|t| ReturnType::Type(Box::new(t)))
}

/// The result of a method that returns the type of its first argument.
#[doc(hidden)]
pub fn returns_self() -> Option<ReturnType> {
    Some(ReturnType::SelfType(SelfMarker::SelfType))
}

/// A method.
///
/// # Panics
///
/// When `name` is not a method name, which the macro checks.
#[doc(hidden)]
pub fn method(
    name: &str,
    description: &str,
    params: Vec<ParamDef>,
    return_type: Option<ReturnType>,
) -> MethodDef {
    MethodDef {
        name: MethodName::new(name).expect("the macro checks the name"),
        description: description.to_string(),
        params,
        return_type,
        stochastic: false,
        has_default_impl: false,
    }
}

/// The field that an accessor marked `#[r8(field)]` gives.
#[doc(hidden)]
pub fn required<T: Encode>(name: &str, description: &str) -> RequiredField {
    RequiredField {
        name: Name::new(name).expect("the macro checks the name"),
        ty: T::field_type(),
        description: description.into(),
    }
}

/// A trait.
#[doc(hidden)]
pub fn trait_def(
    name: &str,
    description: &str,
    tag: Option<&str>,
    generic: bool,
    methods: Vec<MethodDef>,
    required_fields: Vec<RequiredField>,
) -> TraitDef {
    TraitDef {
        name: Name::new(name).expect("the macro checks the name"),
        description: description.to_string(),
        methods,
        required_fields,
        supertraits: Vec::new(),
        tag: tag.map(|t| Name::new(t).expect("the macro checks the tag")),
        generic,
        extra_type_params: 0,
    }
}

/// The declaration of a trait of `#[r8::methods]`, a constant with the
/// trait's name and path, which `traits` of `r8::exports!` lists when Rust
/// declares the trait.
pub struct TraitDecl {
    /// The qualified trait name.
    #[doc(hidden)]
    pub name: &'static str,
    /// Why the trait is not declared in Rust; `None` when it may be.
    #[doc(hidden)]
    pub unsupported: Option<&'static str>,
    /// The declaration.
    #[doc(hidden)]
    pub def: fn() -> TraitDef,
    /// Adds the declarations of the types that the trait names.
    #[doc(hidden)]
    pub register: fn(&mut DocumentSet),
}

impl TraitDecl {
    /// Whether the trait is of the namespace `ns`.
    #[doc(hidden)]
    pub const fn is_of(&self, ns: &str) -> bool {
        let (name, ns) = (self.name.as_bytes(), ns.as_bytes());
        if name.len() < ns.len() + 2 || name[ns.len()] != b':' || name[ns.len() + 1] != b':' {
            return false;
        }
        let mut i = 0;
        while i < ns.len() {
            if name[i] != ns[i] {
                return false;
            }
            i += 1;
        }
        true
    }
}
