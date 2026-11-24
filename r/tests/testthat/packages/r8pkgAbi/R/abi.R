# abi.R
# ::r8pkgAbi::
# 2026- EDG rtemis.org

# The exports table of r8pkgR, changed so that registration refuses it
# (`src/abi.c` lists the changes), as an external pointer; 0 changes nothing.
table <- function(kind) .Call(abi_table, as.integer(kind))

# The status that R8Api.protect returns for a callback that raises an R error.
protect_status <- function() .Call(abi_protect_status)

# The code that R8Api.value_state, or upcast_state with `upcast`, reports for
# this package's read of `x` as a value of `type`; "" when it lends the state.
lend <- function(x, type, upcast = FALSE) .Call(abi_lend, x, type, upcast)

# The code that R8Api.new_value reports for a state of this package wrapped
# as a value of `type`.
wrap <- function(type) .Call(abi_wrap, type)

# The value that R8Api.value_of_fields builds from the list `fields`, or the
# code that it reports.
of_fields <- function(type, fields, hash, variant = NULL) {
  .Call(abi_of_fields, type, variant, hash, fields)
}

# The value that R8Api.with_fields builds from `x` and the named list `fields`,
# or the code and the message that it reports.
with_fields <- function(x, fields) .Call(abi_with_fields, x, fields)

# The code that R8Api.pin reports for the bytes `record` from this package.
pin <- function(record) .Call(abi_pin, record)
