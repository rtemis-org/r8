//! The plan of a call of a table function: a graph of its stages, which a
//! host gives before the call runs, as `r8explain()` does, and which the
//! core writes as a Mermaid flowchart.
//!
//! A plan is a list of nodes, each with the members of rtemis's session
//! records: `node_id` (`s1`, `s2`, ... in the plan's order), `parent_id`,
//! `kind`, `label`, `status`, `t_start`, `t_end`, and `meta`. The first node
//! is the call, of the kind of its function (`r8map`), and every other node
//! is a stage of it, of a kind prefixed `r8.`, whose `meta.inputs` are the
//! stages that it reads, the arrows of the flow. A stage counts calls and
//! rows; it is never one group or one call.
//!
//! spec: R8/raven#plans

use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use crate::canonical::{declaration_hash, function_hash, text_hash};
use crate::ir::Logic;
use crate::message::{Body, Path, Place, Size, counted, started};
use crate::name::QualifiedName;
use crate::rules::TypeLookup;
use crate::rules::calc::Planned;

/// The format of the plan record.
pub const FORMAT: u32 = 1;

/// The status of a node: `pending` before it starts, `running`, and
/// `completed`, `failed`, or `aborted` at its end.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Status {
    /// The node has not started.
    Pending,
    /// The node runs.
    Running,
    /// The node ran to its end.
    Completed,
    /// An error ended the node.
    Failed,
    /// An interrupt or a time limit ended the node.
    Aborted,
}

/// A plan: the call's node, then its stages.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Plan {
    /// The nodes, the call's first.
    pub nodes: Vec<Node>,
}

/// A node of a plan.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Node {
    /// The node's ID, `s1`, `s2`, ... in the plan's order.
    pub node_id: String,
    /// The call's node, for a stage; `null` for the call.
    pub parent_id: Option<String>,
    /// The function, for the call (`r8map`), or the stage, prefixed `r8.`.
    pub kind: String,
    /// What tells the node apart from others of its kind, or `""`.
    #[serde(default)]
    pub label: String,
    /// The node's status.
    pub status: Status,
    /// When the node started, in seconds from the call's start; `null`
    /// before a run.
    #[serde(default)]
    pub t_start: Option<f64>,
    /// When the node ended, in seconds from the call's start; `null` before
    /// a run.
    #[serde(default)]
    pub t_end: Option<f64>,
    /// The facts of the node.
    #[serde(default)]
    pub meta: Meta,
}

/// The facts of a node; a member that does not apply is absent.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[serde(default)]
pub struct Meta {
    /// Where the node runs: `rust` or `r` for a stage, and, for the call,
    /// `rust`, `r`, or `rust+r`, the languages of its evaluation and its
    /// calls of typed functions.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub engine: Option<String>,
    /// The stages that the stage reads.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub inputs: Vec<String>,
    /// Whether the stage may run on worker threads.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub threads: Option<bool>,
    /// The qualified name of the typed function that the stage calls.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub function: Option<String>,
    /// The language of that function's body, `rust` or `r`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub language: Option<String>,
    /// The column that the stage's calls compute.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub column: Option<String>,
    /// The key columns of the groups.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub keys: Vec<String>,
    /// The rows of the table.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rows: Option<u64>,
    /// The columns that the call computes.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub columns: Option<u64>,
    /// The groups of the rows.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub groups: Option<u64>,
    /// The items of a list or a vector.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub items: Option<u64>,
    /// The calls of typed functions.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub calls: Option<u64>,
    /// The aggregates of the dialect.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub aggregates: Option<u64>,
    /// The code that makes the call's calls of typed functions: `rust`, for
    /// a table, or `host`, a loop of the host, for a list.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    /// The format of the plan record, [`FORMAT`].
    #[serde(skip_serializing_if = "Option::is_none")]
    pub format: Option<u32>,
    /// The plan hash.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub plan_hash: Option<String>,
    /// The threads that the call may use.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub n_threads: Option<u64>,
    /// The typed functions that the call calls.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub functions: Vec<FunctionMeta>,
    /// The call, as the host writes it.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub call: Option<String>,
    /// The version of R8 of the host.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub r8_version: Option<String>,
    /// The host's platform.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub platform: Option<String>,
    /// The seconds that the stage was busy in a run: its time may overlap
    /// another stage's, since aggregates and calls interleave.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub busy: Option<f64>,
    /// The group of a failed call, from 1.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub group: Option<u64>,
    /// The first row of the failed group, or the failed row, from 1.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub row: Option<u64>,
    /// The message of a failed stage.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
}

/// A typed function that a call calls.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[serde(default)]
pub struct FunctionMeta {
    /// Its qualified name.
    pub name: String,
    /// The language of its body, `rust` or `r`.
    pub language: String,
    /// The package that declares it, as the host gives it.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub package: Option<String>,
    /// That package's version, as the host gives it.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
    /// The SHA-256 of the body's code, as the host gives it, for a function
    /// that no package declares.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body_hash: Option<String>,
}

/// A call of a table function over the columns of a table.
#[derive(Debug, Clone, Copy)]
pub struct TableCall<'a> {
    /// The function, such as `r8map`.
    pub function: &'a str,
    /// The call's label: the function mapped, or the columns computed.
    pub label: &'a str,
    /// The table's type.
    pub table: &'a QualifiedName,
    /// The key columns.
    pub by: &'a [&'a str],
    /// The columns computed, by the expressions' order.
    pub columns: &'a [&'a str],
    /// The expressions.
    pub exprs: &'a [&'a Logic],
    /// One value for each group.
    pub summary: bool,
    /// The threads that the call may use.
    pub threads: usize,
    /// The host's language, as an engine's name: `r`.
    pub host: &'a str,
}

/// A map of a typed function over the items of a list or a vector, whose
/// calls a loop of the host makes.
#[derive(Debug, Clone, Copy)]
pub struct ListCall<'a> {
    /// The function, such as `r8map`.
    pub function: &'a str,
    /// The typed function mapped.
    pub typed: &'a QualifiedName,
    /// The items.
    pub items: u64,
    /// Whether a package registered the typed function's body in compiled
    /// code.
    pub in_rust: bool,
    /// The host's language, as an engine's name: `r`.
    pub host: &'a str,
}

/// The language of a body: `rust`, or the host's.
fn language(in_rust: bool, host: &str) -> &str {
    if in_rust { "rust" } else { host }
}

/// The nodes of a plan, numbered as they are added; every stage is a child of
/// the call's node.
struct Builder {
    nodes: Vec<Node>,
}

impl Builder {
    fn new(function: &str, label: &str, meta: Meta) -> Builder {
        Builder {
            nodes: vec![Node {
                node_id: "s1".into(),
                parent_id: None,
                kind: function.into(),
                label: label.into(),
                status: Status::Pending,
                t_start: None,
                t_end: None,
                meta,
            }],
        }
    }

    /// Adds a stage of `kind`, run in `engine`, and gives its ID.
    fn stage(
        &mut self,
        kind: &str,
        label: String,
        engine: &str,
        inputs: &[String],
        meta: Meta,
    ) -> String {
        let id = format!("s{}", self.nodes.len() + 1);
        self.nodes.push(Node {
            node_id: id.clone(),
            parent_id: Some("s1".into()),
            kind: format!("r8.{kind}"),
            label,
            status: Status::Pending,
            t_start: None,
            t_end: None,
            meta: Meta {
                engine: Some(engine.into()),
                inputs: inputs.to_vec(),
                threads: Some(meta.threads.unwrap_or(false)),
                ..meta
            },
        });
        id
    }
}

/// The plan of a call over the columns of a table: `check` of its arguments
/// in the host, `compile` of its expressions, `group` of the rows by the keys,
/// `aggregate` of the dialect's aggregates, a `call` stage for each call of a
/// typed function in each expression, `evaluate` of the rows of the
/// expressions of a column of every row, and `assemble` of the result in the
/// host. `in_rust` tells whether a package
/// registered a typed function's body in compiled code.
///
/// A stage's `inputs` are the stages whose results it reads: `compile` and
/// `group` read `check`; `aggregate` reads `compile` and `group`; a `call`
/// reads them and, where its arguments read an aggregate, `aggregate`;
/// `evaluate` reads them, `aggregate` where an expression of rows has
/// aggregates, and the `call` stages of those expressions; `assemble` reads
/// each work stage that no other stage reads, or `compile` and `group` where
/// there is none.
pub fn table_plan(
    call: &TableCall<'_>,
    planned: &Planned,
    lookup: &dyn TypeLookup,
    in_rust: &dyn Fn(&QualifiedName) -> bool,
) -> Plan {
    table_plan_indexed(call, planned, lookup, in_rust).0
}

/// The places of a table plan's stages among its nodes.
#[derive(Debug, Clone, Default)]
pub(crate) struct StageIndex {
    pub(crate) compile: usize,
    pub(crate) group: Option<usize>,
    pub(crate) aggregate: Option<usize>,
    /// The `call` stages of each expression, in the order of its calls.
    pub(crate) calls: Vec<Vec<usize>>,
    pub(crate) evaluate: Option<usize>,
    pub(crate) assemble: usize,
}

/// The place among a plan's nodes of the stage of ID `id`, `s<n>`.
fn place_of(id: &str) -> usize {
    id[1..].parse::<usize>().map_or(0, |n| n - 1)
}

/// [`table_plan`], with the places of its stages.
pub(crate) fn table_plan_indexed(
    call: &TableCall<'_>,
    planned: &Planned,
    lookup: &dyn TypeLookup,
    in_rust: &dyn Fn(&QualifiedName) -> bool,
) -> (Plan, StageIndex) {
    let host = call.host;
    let per_call = planned.groups.unwrap_or(1) as u64;
    let mut functions: Vec<QualifiedName> = Vec::new();
    for e in &planned.exprs {
        for f in &e.calls {
            if !functions.contains(f) {
                functions.push(f.clone());
            }
        }
    }
    let bodies: Vec<Body> = functions
        .iter()
        .map(|f| Body {
            function: f.to_string(),
            in_rust: in_rust(f),
        })
        .collect();
    let place = Place {
        path: Path::Rust,
        bodies: bodies.clone(),
    };
    let calls: u64 = planned
        .exprs
        .iter()
        .map(|e| e.calls.len() as u64)
        .sum::<u64>()
        * per_call;
    let root = Meta {
        engine: Some(place.engine().name().into()),
        path: Some("rust".into()),
        format: Some(FORMAT),
        plan_hash: Some(table_hash(call, &functions, lookup)),
        n_threads: Some(call.threads.max(1) as u64),
        rows: Some(planned.rows as u64),
        columns: Some(call.columns.len() as u64),
        groups: planned.groups.map(|g| g as u64),
        calls: Some(calls),
        functions: bodies
            .iter()
            .map(|b| FunctionMeta {
                name: b.function.clone(),
                language: language(b.in_rust, host).into(),
                ..FunctionMeta::default()
            })
            .collect(),
        ..Meta::default()
    };
    let mut b = Builder::new(call.function, call.label, root);
    let check = b.stage("check", String::new(), host, &[], Meta::default());
    let compile = b.stage(
        "compile",
        String::new(),
        "rust",
        std::slice::from_ref(&check),
        Meta {
            columns: Some(call.columns.len() as u64),
            ..Meta::default()
        },
    );
    let mut index = StageIndex {
        compile: place_of(&compile),
        ..StageIndex::default()
    };
    let mut read = vec![compile.clone()];
    if !call.by.is_empty() {
        let group = b.stage(
            "group",
            format!("by {}", call.by.join(", ")),
            "rust",
            std::slice::from_ref(&check),
            Meta {
                keys: call.by.iter().map(|k| k.to_string()).collect(),
                rows: Some(planned.rows as u64),
                groups: planned.groups.map(|g| g as u64),
                ..Meta::default()
            },
        );
        index.group = Some(place_of(&group));
        read.push(group);
    }
    let with = |extra: &[String]| -> Vec<String> {
        let mut v = read.clone();
        v.extend(extra.iter().cloned());
        v
    };
    let mut work: Vec<String> = Vec::new();
    let mut read_by_work: Vec<String> = Vec::new();
    let aggregates: usize = planned.exprs.iter().map(|e| e.aggregates).sum();
    let aggregate = (aggregates > 0).then(|| {
        let kernels = planned.exprs.iter().any(|e| e.kernels > 0);
        b.stage(
            "aggregate",
            String::new(),
            "rust",
            &read,
            Meta {
                aggregates: Some(aggregates as u64),
                threads: Some(kernels && planned.kernel_threads > 1),
                ..Meta::default()
            },
        )
    });
    index.aggregate = aggregate.as_deref().map(place_of);
    work.extend(aggregate.iter().cloned());
    // The call stages of each expression, for the stage that evaluates its
    // rows.
    let mut calls_of: Vec<Vec<String>> = Vec::with_capacity(planned.exprs.len());
    for (e, column) in planned.exprs.iter().zip(call.columns) {
        let mut ids = Vec::with_capacity(e.calls.len());
        for (k, f) in e.calls.iter().enumerate() {
            let lang = language(in_rust(f), host);
            // A function called twice in one expression has an ordinal.
            let twice = e.calls.iter().filter(|g| *g == f).count() > 1;
            let nth = e.calls[..=k].iter().filter(|g| *g == f).count();
            let label = if twice {
                format!("{f}: {column} #{nth}")
            } else {
                format!("{f}: {column}")
            };
            let over_aggregate: Vec<String> = match (&aggregate, e.call_reads_aggregate[k]) {
                (Some(a), true) => vec![a.clone()],
                _ => Vec::new(),
            };
            read_by_work.extend(over_aggregate.iter().cloned());
            let id = b.stage(
                "call",
                label,
                lang,
                &with(&over_aggregate),
                Meta {
                    function: Some(f.to_string()),
                    language: Some(lang.into()),
                    column: Some(column.to_string()),
                    calls: Some(per_call),
                    ..Meta::default()
                },
            );
            ids.push(id.clone());
            work.push(id);
        }
        index.calls.push(ids.iter().map(|i| place_of(i)).collect());
        calls_of.push(ids);
    }
    if planned.exprs.iter().any(|e| e.rows) {
        let mut extra: Vec<String> = Vec::new();
        if let Some(a) = &aggregate
            && planned.exprs.iter().any(|e| e.rows && e.aggregates > 0)
        {
            extra.push(a.clone());
        }
        for (e, ids) in planned.exprs.iter().zip(&calls_of) {
            if e.rows {
                extra.extend(ids.iter().cloned());
            }
        }
        read_by_work.extend(extra.iter().cloned());
        let evaluate = b.stage(
            "evaluate",
            String::new(),
            "rust",
            &with(&extra),
            Meta {
                rows: Some(planned.rows as u64),
                ..Meta::default()
            },
        );
        index.evaluate = Some(place_of(&evaluate));
        work.push(evaluate);
    }
    let last: Vec<String> = work
        .into_iter()
        .filter(|w| !read_by_work.contains(w))
        .collect();
    let assembled = if last.is_empty() { read } else { last };
    let assemble = b.stage("assemble", String::new(), host, &assembled, Meta::default());
    index.assemble = place_of(&assemble);
    (Plan { nodes: b.nodes }, index)
}

/// The plan of a call that failed before its expressions compiled: the call,
/// `check`, and the stage that failed, `group` of a key or `compile` of an
/// expression, with its message; a table that does not read fails the call
/// alone.
pub(crate) fn failed_plan(call: &TableCall<'_>, stage: Option<&str>, message: &str) -> Plan {
    let host = call.host;
    let root = Meta {
        path: Some("rust".into()),
        format: Some(FORMAT),
        n_threads: Some(call.threads.max(1) as u64),
        message: stage.is_none().then(|| message.to_string()),
        ..Meta::default()
    };
    let mut b = Builder::new(call.function, call.label, root);
    b.nodes[0].status = Status::Failed;
    let check = b.stage("check", String::new(), host, &[], Meta::default());
    b.nodes[1].status = Status::Completed;
    if let Some(stage) = stage {
        let label = if stage == "group" {
            format!("by {}", call.by.join(", "))
        } else {
            String::new()
        };
        b.stage(
            stage,
            label,
            "rust",
            std::slice::from_ref(&check),
            Meta {
                message: Some(message.to_string()),
                ..Meta::default()
            },
        );
        b.nodes[2].status = Status::Failed;
    }
    Plan { nodes: b.nodes }
}

/// The plan of a map over the items of a list or a vector: `check` of its
/// arguments, `call` of the typed function for each item, and `assemble` of
/// the results, the host's loop making the calls.
pub fn list_plan(call: &ListCall<'_>, lookup: &dyn TypeLookup) -> Plan {
    let body = Body {
        function: call.typed.to_string(),
        in_rust: call.in_rust,
    };
    let place = Place {
        path: Path::Host,
        bodies: vec![body],
    };
    let lang = language(call.in_rust, call.host);
    let hash = text_hash(&json!({
        "format": FORMAT,
        "function": call.function,
        "over": "items",
        "functions": {call.typed.to_string(): typed_hash(call.typed, lookup)},
    }));
    let root = Meta {
        engine: Some(place.engine().name().into()),
        path: Some("host".into()),
        format: Some(FORMAT),
        plan_hash: Some(hash),
        n_threads: Some(1),
        items: Some(call.items),
        calls: Some(call.items),
        functions: vec![FunctionMeta {
            name: call.typed.to_string(),
            language: lang.into(),
            ..FunctionMeta::default()
        }],
        ..Meta::default()
    };
    let mut b = Builder::new(call.function, &call.typed.to_string(), root);
    let check = b.stage("check", String::new(), call.host, &[], Meta::default());
    let calls = b.stage(
        "call",
        call.typed.to_string(),
        lang,
        std::slice::from_ref(&check),
        Meta {
            function: Some(call.typed.to_string()),
            language: Some(lang.into()),
            items: Some(call.items),
            calls: Some(call.items),
            ..Meta::default()
        },
    );
    b.stage(
        "assemble",
        String::new(),
        call.host,
        &[calls],
        Meta::default(),
    );
    Plan { nodes: b.nodes }
}

/// The hash of a typed function's declaration, or `null` where the lookup
/// holds none.
fn typed_hash(f: &QualifiedName, lookup: &dyn TypeLookup) -> Value {
    lookup
        .function_of(f)
        .map_or(Value::Null, |def| Value::String(function_hash(def, lookup)))
}

/// The plan hash of a call over a table: the SHA-256 of the canonical text of
/// an object of `format`, `function`, `table` (the table type's declaration
/// hash), `by`, `summary`, `expressions` (each column's expression in the
/// dialect's JSON form, by the column), and `functions` (each typed function's
/// declaration hash, by its qualified name).
fn table_hash(
    call: &TableCall<'_>,
    functions: &[QualifiedName],
    lookup: &dyn TypeLookup,
) -> String {
    let exprs: serde_json::Map<String, Value> = call
        .columns
        .iter()
        .zip(call.exprs)
        .map(|(c, e)| (c.to_string(), e.to_value()))
        .collect();
    let fns: serde_json::Map<String, Value> = functions
        .iter()
        .map(|f| (f.to_string(), typed_hash(f, lookup)))
        .collect();
    text_hash(&json!({
        "format": FORMAT,
        "function": call.function,
        "table": declaration_hash(call.table, lookup),
        "by": call.by,
        "summary": call.summary,
        "expressions": exprs,
        "functions": fns,
    }))
}

/// The headline of a plan, in the words of the start line of a long map:
/// `r8map(): pkgr::spread over 10 columns and 3 groups, 30 calls, in Rust`.
pub fn headline(plan: &Plan, host: &str) -> String {
    let Some(root) = plan.nodes.first() else {
        return String::new();
    };
    let m = &root.meta;
    let place = Place {
        path: if m.path.as_deref() == Some("host") {
            Path::Host
        } else {
            Path::Rust
        },
        bodies: m
            .functions
            .iter()
            .map(|f| Body {
                function: f.name.clone(),
                in_rust: f.language == "rust",
            })
            .collect(),
    };
    let size = Size {
        columns: m.columns,
        groups: m.groups,
        items: m.items,
        calls: m.calls.filter(|&n| n > 0),
    };
    started(&root.kind, &root.label, &size, &place, host)
}

/// The text of each stage of a plan, as its flowchart writes it.
pub fn stage_texts(plan: &Plan) -> Vec<String> {
    plan.nodes.iter().skip(1).map(stage_text).collect()
}

/// The text of a stage in a flowchart: its kind, its label, and its count.
fn stage_text(n: &Node) -> String {
    let short = n.kind.strip_prefix("r8.").unwrap_or(&n.kind);
    let mut text = short.to_string();
    if !n.label.is_empty() {
        text.push(' ');
        text.push_str(&n.label);
    }
    let m = &n.meta;
    let count = match short {
        "compile" => m.columns.map(|c| counted(c, "expression", "expressions")),
        "group" => m.groups.map(|g| counted(g, "group", "groups")),
        "evaluate" => m.rows.map(|r| counted(r, "row", "rows")),
        "aggregate" => m.aggregates.map(|a| counted(a, "aggregate", "aggregates")),
        "call" => m.calls.map(|c| counted(c, "call", "calls")),
        _ => None,
    };
    if let Some(c) = count {
        text.push_str(", ");
        text.push_str(&c);
    }
    text
}

/// An engine's name in a text: `Rust`, or the host's language, `R`.
fn engine_name(engine: &str) -> String {
    match engine {
        "rust" => "Rust".to_string(),
        other => other.to_uppercase(),
    }
}

/// A label of a flowchart, with its double quotes as Mermaid's entity.
fn quoted(text: &str) -> String {
    format!("\"{}\"", text.replace('"', "#quot;").replace('\n', " "))
}

/// The Mermaid flowchart of a plan: the call as a subgraph of its stages,
/// each of the class of its engine (`rust` or `r`), and an arrow from each
/// stage that a stage reads. After a run, a stage that failed, was aborted,
/// or did not start takes the class of its status (`failed`, `aborted`,
/// `pending`), and its text ends with its engine, `(Rust)` or `(R)`. The
/// text declares no `classDef`; the host that draws it styles the classes.
pub fn mermaid(plan: &Plan) -> String {
    let mut out = String::from("flowchart LR\n");
    let Some(root) = plan.nodes.first() else {
        return out;
    };
    let ran = root.status != Status::Pending;
    out.push_str(&format!(
        "  subgraph {}[{}]\n",
        root.node_id,
        quoted(&format!("{}(): {}", root.kind, root.label))
    ));
    for n in &plan.nodes[1..] {
        let engine = n.meta.engine.as_deref().unwrap_or_default();
        let status = match n.status {
            Status::Failed if ran => Some("failed"),
            Status::Aborted if ran => Some("aborted"),
            Status::Pending if ran => Some("pending"),
            _ => None,
        };
        let mut text = stage_text(n);
        if status.is_some() {
            text.push_str(&format!(" ({})", engine_name(engine)));
        }
        out.push_str(&format!(
            "    {}[{}]:::{}\n",
            n.node_id,
            quoted(&text),
            status.unwrap_or(engine)
        ));
    }
    out.push_str("  end\n");
    for n in &plan.nodes[1..] {
        for i in &n.meta.inputs {
            out.push_str(&format!("  {i} --> {}\n", n.node_id));
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn node(id: &str, kind: &str, label: &str, engine: &str, inputs: &[&str]) -> Node {
        Node {
            node_id: id.into(),
            parent_id: Some("s1".into()),
            kind: kind.into(),
            label: label.into(),
            status: Status::Pending,
            t_start: None,
            t_end: None,
            meta: Meta {
                engine: Some(engine.into()),
                inputs: inputs.iter().map(|s| s.to_string()).collect(),
                ..Meta::default()
            },
        }
    }

    fn plan() -> Plan {
        let mut root = node("s1", "r8map", "pkgr::spread", "rust", &[]);
        root.parent_id = None;
        let mut call = node("s4", "r8.call", "pkgr::spread: \"sbp\"", "rust", &["s3"]);
        call.meta.calls = Some(3);
        Plan {
            nodes: vec![
                root,
                node("s2", "r8.check", "", "r", &[]),
                node("s3", "r8.compile", "", "rust", &["s2"]),
                call,
                node("s5", "r8.assemble", "", "r", &["s4"]),
            ],
        }
    }

    #[test]
    fn a_plan_writes_a_flowchart_of_its_stages() {
        assert_eq!(
            mermaid(&plan()),
            "flowchart LR\n  subgraph s1[\"r8map(): pkgr::spread\"]\n    s2[\"check\"]:::r\n    s3[\"compile\"]:::rust\n    s4[\"call pkgr::spread: #quot;sbp#quot;, 3 calls\"]:::rust\n    s5[\"assemble\"]:::r\n  end\n  s2 --> s3\n  s3 --> s4\n  s4 --> s5\n"
        );
    }

    #[test]
    fn after_a_run_a_stage_takes_the_class_of_its_status() {
        let mut p = plan();
        p.nodes[0].status = Status::Failed;
        p.nodes[1].status = Status::Completed;
        p.nodes[2].status = Status::Completed;
        p.nodes[3].status = Status::Failed;
        let text = mermaid(&p);
        assert!(text.contains("s2[\"check\"]:::r\n"));
        assert!(text.contains(":::failed\n"));
        assert!(
            text.contains("s4[\"call pkgr::spread: #quot;sbp#quot;, 3 calls (Rust)\"]:::failed\n")
        );
        assert!(text.contains("s5[\"assemble (R)\"]:::pending\n"));
    }

    #[test]
    fn statuses_read_and_write_as_their_names() {
        let p = plan();
        let back: Plan = serde_json::from_value(serde_json::to_value(&p).unwrap()).unwrap();
        assert_eq!(back, p);
        assert_eq!(
            serde_json::to_value(Status::Completed).unwrap(),
            "completed"
        );
        assert_eq!(p.nodes[0].meta.threads, None);
    }
}
