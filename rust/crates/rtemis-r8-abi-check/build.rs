//! Compiles `csrc/struct_offsets.c` against `r/inst/include/R8.h`. The stub headers
//! in `csrc/stub` declare the two R types the header uses, so the check runs
//! without an R installation.

fn main() {
    println!("cargo:rerun-if-changed=csrc");
    println!("cargo:rerun-if-changed=../../../r/inst/include/R8.h");
    cc::Build::new()
        .file("csrc/struct_offsets.c")
        .include("../../../r/inst/include")
        .include("csrc/stub")
        .warnings_into_errors(true)
        .compile("r8_abi_struct_offsets");
}
