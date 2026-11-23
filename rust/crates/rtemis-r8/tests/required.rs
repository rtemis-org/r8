//! Required fields of traits in the registry: a redeclared struct or trait
//! that leaves an impl's struct without a required field is refused, and the
//! registry keeps its declarations.
//! spec: R8/class-system#required-fields-of-traits

use rtemis_r8::ir::{ImplDef, TraitDef, TypeDef};
use rtemis_r8::name::Name;
use rtemis_r8::registry::Registry;
use serde_json::json;

fn name(s: &str) -> Name {
    Name::new(s).unwrap()
}

fn range(fields: serde_json::Value) -> TypeDef {
    serde_json::from_value(
        json!({"kind": "struct", "name": "Range", "version": 1, "fields": fields}),
    )
    .unwrap()
}

fn measured(required: serde_json::Value) -> TraitDef {
    serde_json::from_value(
        json!({"name": "Measured", "methods": [{"name": "span", "params": ["x"]}],
        "required_fields": required}),
    )
    .unwrap()
}

fn codes(out: &rtemis_r8::registry::Outcome) -> Vec<&'static str> {
    out.problems.iter().map(|e| e.code).collect()
}

#[test]
fn a_redeclaration_keeps_every_impl_s_required_fields() {
    let r = Registry::new();
    let ns = name("ns");
    let lo_hi = json!([{"name": "lo", "type": {"kind": "double"}}, {"name": "hi", "type": {"kind": "double"}}]);
    assert!(codes(&r.declare_type(&ns, range(lo_hi.clone())).unwrap()).is_empty());
    let lo = json!([{"name": "lo", "type": {"kind": "double"}}]);
    assert!(codes(&r.declare_trait(&ns, measured(lo.clone())).unwrap()).is_empty());
    let imp: ImplDef = serde_json::from_value(json!({"trait": "ns::Measured",
        "target": {"kind": "type", "name": "ns::Range"}, "methods": []}))
    .unwrap();
    assert!(codes(&r.declare_impl(&ns, imp).unwrap()).is_empty());
    // The struct without `lo` is refused, and the registry keeps `Range`.
    let out = r
        .declare_type(
            &ns,
            range(json!([{"name": "hi", "type": {"kind": "double"}}])),
        )
        .unwrap();
    assert_eq!(codes(&out), ["required_field_missing"]);
    assert!(!out.changed);
    // The trait that also requires `mid` is refused.
    let out = r
        .declare_trait(
            &ns,
            measured(json!([{"name": "lo", "type": {"kind": "double"}},
                {"name": "mid", "type": {"kind": "double"}}])),
        )
        .unwrap();
    assert_eq!(codes(&out), ["required_field_missing"]);
    // The same struct declared again is accepted.
    assert!(codes(&r.declare_type(&ns, range(lo_hi)).unwrap()).is_empty());
}
