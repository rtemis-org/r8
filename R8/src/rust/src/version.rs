//! Version of the `rtemis-r8` crate compiled into the package.

use extendr_api::prelude::*;

/// Version of the rtemis-r8 crate
///
/// Returns the version of the `rtemis-r8` Rust crate compiled into R8, which
/// provenance records alongside the versions of R8 and R.
///
/// @return Character: The version, for example `"0.1.0"`.
///
/// @examples
/// r8_core_version()
///
/// @export
#[extendr]
fn r8_core_version() -> &'static str {
    rtemis_r8::VERSION
}

extendr_module! {
    mod version;
    fn r8_core_version;
}
