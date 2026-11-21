//! R bindings of the `rtemis-r8` crate for the R package R8.
//!
//! Each module converts R objects to the values `rtemis-r8` takes, checks the R
//! input contract, and reports errors with R argument names and one-based
//! positions.
//!
//! The crate is `r8-r`, internal to the R package. The extendr module is `R8`
//! because R calls the package's initialization routine `R_init_R8` (see
//! `entrypoint.c`), named after the R package.

use extendr_api::prelude::*;

mod alloc;
mod construct;
mod declare;
mod exports;
mod irde;
mod irser;
mod native;
#[cfg(feature = "oracle")]
mod oracle;
mod rview;
mod text;
mod values;
mod version;

// The feature `oracle` adds the differential oracle's routines (`oracle.rs`).
#[cfg(not(feature = "oracle"))]
extendr_module! {
    mod R8;
    use alloc;
    use construct;
    use declare;
    use exports;
    use native;
    use text;
    use values;
    use version;
}

#[cfg(feature = "oracle")]
extendr_module! {
    mod R8;
    use alloc;
    use construct;
    use declare;
    use exports;
    use native;
    use oracle;
    use text;
    use values;
    use version;
}
