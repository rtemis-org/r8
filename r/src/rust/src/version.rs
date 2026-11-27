//! Version of the `rtemis-r8` crate compiled into the package.

use extendr_api::prelude::*;

/// Version of the R8 runtime
///
/// Returns the version of the runtime compiled into R8, the Rust crate
/// `rtemis-r8`.
///
/// @return Character: The version, for example `"0.1.0"`.
///
/// @examples
/// r8runtime_version()
///
/// @export
#[extendr]
fn r8runtime_version() -> &'static str {
    rtemis_r8::VERSION
}

extendr_module! {
    mod version;
    fn r8runtime_version;
}
