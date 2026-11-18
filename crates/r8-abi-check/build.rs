//! Compiles `csrc/layout.c` against `R8/inst/include/R8.h`. The stub headers
//! in `csrc/stub` declare the two R types the header uses, so the check runs
//! without an R installation.

fn main() {
    println!("cargo:rerun-if-changed=csrc");
    println!("cargo:rerun-if-changed=../../R8/inst/include/R8.h");
    cc::Build::new()
        .file("csrc/layout.c")
        .include("../../R8/inst/include")
        .include("csrc/stub")
        .warnings_into_errors(true)
        .compile("r8_abi_layout");
}
