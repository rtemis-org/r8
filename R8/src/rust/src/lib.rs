//! R bindings of the `r8-core` crate for the R package R8.
//!
//! Each module converts R objects to the values `r8-core` takes, checks the R
//! input contract, and reports errors with R argument names and one-based
//! positions.
//!
//! The crate is `r8-r`, internal to the R package. The extendr module is `R8`
//! because R calls the package's initialization routine `R_init_R8` (see
//! `entrypoint.c`), named after the R package.

use extendr_api::prelude::*;

mod declare;
mod values;
mod version;

extendr_module! {
    mod R8;
    use declare;
    use values;
    use version;
}
