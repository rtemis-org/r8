//! The fit of one field type to another: a type that a declared type
//! admits, by the relation that required fields of traits use, and that the
//! return types of methods use.
//! spec: R8/class-system#required-fields-of-traits

use super::types::FieldType;
use crate::rules::TypeLookup;

/// `true` when a value of type `have` is always a value of type `want`:
/// the two are equal; `want` is `option(T)` and `have` is `T'` or
/// `option(T')` with `T'` fitting `T`; `want` is `subtype(P)` and `have`
/// is a struct type, or `subtype()` of one, that is a subtype of `P`; or
/// `want` is `dyn(Tr)` and `have` is a type that implements `Tr`, or
/// `dyn(Tr)` itself. A constraint narrows nothing: `type_double(min = 0)`
/// fits `type_double(min = 0)` only.
pub fn fits(lookup: &dyn TypeLookup, have: &FieldType, want: &FieldType) -> bool {
    if have == want {
        return true;
    }
    match want {
        FieldType::Option { inner: w } => match have {
            FieldType::Option { inner: h } => fits(lookup, h, w),
            h => fits(lookup, h, w),
        },
        FieldType::Subtype { of } => match have {
            FieldType::Named { name } | FieldType::Subtype { of: name } => {
                lookup.is_subtype(name, of)
            }
            _ => false,
        },
        FieldType::Dyn { r#trait } => match have {
            FieldType::Named { name } => lookup.implements(r#trait, name).is_some(),
            _ => false,
        },
        _ => false,
    }
}
