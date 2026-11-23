# generate.R
# ::R8::
# 2026- EDG rtemis.org

# The generator of R/r8-types.R: the R declarations of the types a package
# declares in Rust, read from the IR document of the package's compiled
# exports (R8.h). Each field type, rule, and default renders as the R
# declaration call that compiles to it; JSON without an R form renders as
# json_ir(). The generated file is checked by declaring it and comparing the
# declaration hash of every type with the exports' hash.
# spec: R8/class-system#types-declared-in-rust

# %% r8_document_rust ----
#' Generate the R declarations of types declared in Rust
#'
#' Writes `R/r8-types.R`, the R declaration of each type that a package
#' declares in Rust and lists in its compiled exports (`R8.h`): one
#' `new_struct()` or `new_enum()` call per type, Rust-stored types with
#' `storage = "rust"`, with roxygen comments from the Rust doc comments. The R
#' code of the package then has the type objects, their constructors, and
#' their declarations at install, before the shared object loads, and
#' `r8_register()` checks at load that each R declaration has the declaration
#' hash of its Rust declaration.
#'
#' The generator reads the IR document from the compiled shared object,
#' `src/<package><dynlib.ext>`, which it loads with `dyn.load()` and unloads
#' when it finishes; the package itself is not loaded, so the generator runs
#' while its R declarations are absent or differ from the Rust ones. Before it
#' writes, it declares the rendered file in the session and compares the
#' declaration hash of every type with the exports' hash, so it runs in an R
#' session of its own, without the package loaded:
#' `Rscript -e "R8::r8_document_rust()"`, after `pkgbuild::compile_dll()`
#' and before roxygen.
#'
#' @param path Character: The package's root directory.
#' @param check Logical: Compare the rendered file with `R/r8-types.R` and
#'   signal an error when they differ, without writing.
#'
#' The file is current when its code and comments, token by token, equal the
#' rendered file's; a formatter that changes only the layout, such as air,
#' leaves it current, and the generator does not rewrite it.
#'
#' @return The path of `R/r8-types.R`, invisibly.
#'
#' @examples
#' \dontrun{
#' pkgbuild::compile_dll()
#' system2("Rscript", c("-e", shQuote("R8::r8_document_rust()")))
#' roxygen2::roxygenise()
#' }
#'
#' @export
r8_document_rust <- function(path = ".", check = FALSE) {
  desc <- read.dcf(
    file.path(path, "DESCRIPTION"),
    fields = c("Package", "Config/R8/namespace")
  )
  pkg <- unname(desc[1L, "Package"])
  namespace <- unname(desc[1L, "Config/R8/namespace"])
  if (is.na(namespace)) {
    namespace <- pkg
  }
  if (isNamespaceLoaded(pkg)) {
    r8_abort(
      "r8_document_rust() runs in an R session without '",
      pkg,
      "' loaded: Rscript -e \"R8::r8_document_rust()\".",
      class = "r8_declaration_error"
    )
  }
  if (!pkg %in% names(getLoadedDLLs())) {
    so <- file.path(path, "src", paste0(pkg, .Platform[["dynlib.ext"]]))
    if (!file.exists(so)) {
      r8_abort(
        "The shared object ",
        so,
        " of '",
        pkg,
        "' is compiled first: pkgbuild::compile_dll().",
        class = "r8_declaration_error"
      )
    }
    # The package's R_init_<pkg>() registers `r8_exports` with
    # R_RegisterCCallable().
    dyn.load(so)
    on.exit(dyn.unload(so), add = TRUE)
  }
  ptr <- tryCatch(
    .Call(.r8_native[["exports"]], pkg),
    error = function(e) NULL
  )
  if (is.null(ptr)) {
    r8_abort(
      "Package '",
      pkg,
      "' registers no R8 exports (R_RegisterCCallable(\"",
      pkg,
      "\", \"r8_exports\", ...)).",
      class = "r8_declaration_error"
    )
  }
  doc <- r8_exports_document(ptr, namespace)
  .r8_render_refs[["map"]] <- foreign_refs(doc, path)
  on.exit(.r8_render_refs[["map"]] <- NULL, add = TRUE)
  codecs <- r8_exports_compare(ptr, namespace)
  codecs <- codecs[["type_name"]][codecs[["codec"]]]
  lines <- render_document(doc, pkg, codecs)
  verify_document(lines, doc[["namespace"]], ptr)
  file <- file.path(path, "R", "r8-types.R")
  old <- if (file.exists(file)) readLines(file, warn = FALSE) else character()
  # A formatter such as air changes the file's layout and none of its tokens.
  current <- same_code(old, lines)
  if (check) {
    if (!current) {
      r8_abort(
        file,
        " differs from the declarations in the compiled exports of '",
        pkg,
        "'; R8::r8_document_rust() regenerates it.",
        class = "r8_declaration_error"
      )
    }
  } else if (!current) {
    writeLines(lines, file)
  }
  invisible(file)
} # /R8::r8_document_rust


# The R objects of the types of other namespaces that the rendered file
# names, by qualified name, while r8_document_rust() renders.
.r8_render_refs <- new.env(parent = emptyenv())


# %% foreign_refs ----
#' The R objects of the types of other namespaces that a document names
#'
#' For each type of another namespace that the document's types reach, the
#' exported binding of the package that owns the namespace, as R source:
#' `"r8resample::Resampler"`. The owner is found among the packages of the
#' `Imports` and `Depends` of the package's `DESCRIPTION`, each loaded, by
#' R8's registry of namespace claims.
#'
#' @param doc List: The IR document.
#' @param path Character: The package's root directory.
#'
#' @return Named character: R source by qualified name.
#'
#' @keywords internal
#' @noRd
foreign_refs <- function(doc, path) {
  prefix <- paste0(doc[["namespace"]], "::")
  refs <- unique(unlist(lapply(doc[["types"]], named_refs)))
  refs <- refs[!startsWith(refs, prefix)]
  if (length(refs) == 0L) {
    return(character())
  }
  fields <- read.dcf(
    file.path(path, "DESCRIPTION"),
    fields = c("Imports", "Depends")
  )
  deps <- unlist(strsplit(paste(fields[!is.na(fields)], collapse = ","), ","))
  deps <- trimws(sub("\\(.*", "", deps))
  deps <- setdiff(deps[nzchar(deps)], "R")
  for (d in deps) {
    loadNamespace(d)
  }
  out <- character()
  for (q in refs) {
    ns <- sub("::.*", "", q)
    owner <- r8_namespace_owner(ns)
    if (!nzchar(owner) || !owner %in% deps) {
      r8_abort(
        "The type `",
        q,
        "` belongs to the namespace `",
        ns,
        "`, which no package of the Imports of the package's DESCRIPTION owns; ",
        "the package that declares it is listed in Imports.",
        class = "r8_declaration_error"
      )
    }
    binding <- Filter(
      function(e) {
        v <- getExportedValue(owner, e)
        inherits(v, "r8_type") && identical(attr(v, "r8_name"), q)
      },
      getNamespaceExports(owner)
    )
    if (length(binding) == 0L) {
      r8_abort(
        "Package '",
        owner,
        "' owns the namespace `",
        ns,
        "` and exports no type object of `",
        q,
        "`.",
        class = "r8_declaration_error"
      )
    }
    out[[q]] <- paste0(owner, "::", backtick(sort(binding)[[1L]]))
  }
  out
} # /R8::foreign_refs


# %% same_code ----
#' Whether two R sources hold the same tokens
#'
#' @param a,b Character: The lines of each source.
#'
#' @return Logical: `TRUE` when both parse and their terminal tokens,
#'   comments included, are the same in the same order.
#'
#' @keywords internal
#' @noRd
same_code <- function(a, b) {
  tokens <- function(lines) {
    exprs <- tryCatch(
      parse(text = lines, keep.source = TRUE),
      error = function(e) NULL
    )
    if (is.null(exprs)) {
      return(NULL)
    }
    pd <- utils::getParseData(exprs)
    if (is.null(pd)) {
      return(character())
    }
    pd <- pd[pd[["terminal"]], ]
    pd <- pd[order(pd[["line1"]], pd[["col1"]]), ]
    trimws(pd[["text"]], which = "right")
  }
  ta <- tokens(a)
  !is.null(ta) && identical(ta, tokens(b))
} # /R8::same_code


# %% verify_document ----
#' Check that a rendered file reproduces the exports
#'
#' Declares the rendered declarations in the session, in the exports'
#' namespace, and compares the declaration hash of every type of the exports
#' with the hash of its R declaration.
#'
#' @param lines Character: The rendered file.
#' @param namespace Character: The exports' namespace.
#' @param ptr The external pointer that `r8_exports` returned.
#'
#' @return `NULL`, invisibly; signals `r8_declaration_error` naming each type
#'   whose rendering differs.
#'
#' @keywords internal
#' @noRd
verify_document <- function(lines, namespace, ptr) {
  env <- new.env(parent = globalenv())
  op <- options(r8.namespace = namespace)
  on.exit(options(op), add = TRUE)
  eval(parse(text = lines, keep.source = FALSE), envir = env)
  compared <- r8_exports_compare(ptr, namespace)
  bad <- compared[["type_name"]][compared[["status"]] != "same"]
  if (length(bad) > 0L) {
    r8_abort(
      "The rendered R declarations of `",
      paste(bad, collapse = "`, `"),
      "` differ from the declarations in the compiled exports; R8 has no R ",
      "rendering for some part of them.",
      class = "r8_declaration_error"
    )
  }
  invisible(NULL)
} # /R8::verify_document


# %% render_document ----
#' The lines of R/r8-types.R
#'
#' @param doc List: The IR document.
#' @param pkg Character: The package.
#' @param codecs Character: The qualified names of the types with a codec.
#'
#' @return Character: The lines.
#'
#' @keywords internal
#' @noRd
render_document <- function(doc, pkg, codecs) {
  ns <- doc[["namespace"]]
  types <- type_order(doc[["types"]], ns)
  blocks <- lapply(types, function(t) {
    rust_stored <- paste0(ns, "::", t[["name"]]) %in% codecs
    if (identical(t[["kind"]], "struct")) {
      render_struct(t, ns, rust_stored)
    } else {
      render_enum(t, ns, rust_stored)
    }
  })
  text <- c(
    "# r8-types.R",
    paste0("# ::", pkg, "::"),
    "# R8::r8_document_rust() writes this file from the R8 declarations in",
    "# the package's Rust code, and replaces it when they change.",
    unlist(lapply(blocks, function(b) c("", b)))
  )
  # One element per line: a rendered argument spans several lines.
  unlist(lapply(text, function(l) {
    if (nzchar(l)) strsplit(l, "\n", fixed = TRUE)[[1L]] else ""
  }))
} # /R8::render_document


# %% type_order ----
#' Types in an order that declares each after the types it names
#'
#' @param types List: The IR type declarations.
#' @param ns Character: The namespace.
#'
#' @return List: The declarations, reordered.
#'
#' @keywords internal
#' @noRd
type_order <- function(types, ns) {
  names <- vapply(types, function(t) t[["name"]], character(1))
  deps <- lapply(types, function(t) {
    refs <- unique(named_refs(t))
    local <- startsWith(refs, paste0(ns, "::"))
    intersect(substring(refs[local], nchar(ns) + 3L), names)
  })
  names(deps) <- names
  out <- character()
  visit <- function(n, path) {
    if (n %in% out) {
      return(invisible(NULL))
    }
    if (n %in% path) {
      r8_abort(
        "The types `",
        paste(c(path, n), collapse = "` -> `"),
        "` name each other in a cycle.",
        class = "r8_declaration_error"
      )
    }
    for (d in deps[[n]]) {
      visit(d, c(path, n))
    }
    out <<- c(out, n)
  }
  for (n in names) {
    visit(n, character())
  }
  types[match(out, names)]
} # /R8::type_order


# %% named_refs ----
#' The qualified names an IR value refers to
#'
#' @param x An IR value: a declaration, field, or field type.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
named_refs <- function(x) {
  if (!is.list(x)) {
    return(character())
  }
  own <- character()
  kind <- x[["kind"]]
  if (is.character(kind) && kind %in% c("named", "matrix_of", "table_of")) {
    own <- if (kind == "named") x[["name"]] else x[["of"]]
  }
  if (identical(x[["shape"]], "newtype")) {
    own <- x[["of"]]
  }
  c(own, unlist(lapply(unname(x), named_refs)))
} # /R8::named_refs


# %% check_renderable ----
#' Abort for IR members that have no argument in the R declaration calls
#'
#' @param x List: An IR declaration.
#' @param members Character: The members to check.
#' @param what Character: The declaration, for the message.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#' @noRd
check_renderable <- function(x, members, what) {
  present <- members[vapply(
    members,
    function(m) {
      v <- x[[m]]
      !is.null(v) && !identical(v, FALSE) && length(v) > 0L
    },
    logical(1)
  )]
  if (length(present) > 0L) {
    r8_abort(
      what,
      " declares `",
      paste(present, collapse = "`, `"),
      "`, which the R declaration calls do not take.",
      class = "r8_declaration_error"
    )
  }
  invisible(NULL)
} # /R8::check_renderable


# %% render_struct ----
#' The lines of a struct's declaration
#'
#' @param t List: The IR declaration.
#' @param ns Character: The namespace.
#' @param rust_stored Logical: The type has a codec.
#'
#' @return Character: The lines.
#'
#' @keywords internal
#' @noRd
render_struct <- function(t, ns, rust_stored) {
  qname <- paste0(ns, "::", t[["name"]])
  check_renderable(
    t,
    c("extends", "migrations", "aliases", "provenance", "extensions"),
    paste0("`", qname, "`")
  )
  fields <- t[["fields"]]
  params <- vapply(
    fields,
    function(f) {
      d <- f[["description"]]
      paste0(
        "#' @param ",
        f[["name"]],
        " ",
        if (is.null(d) || !nzchar(d)) {
          paste0("The field `", f[["name"]], "`.")
        } else {
          roxygen_text(d)
        }
      )
    },
    character(1)
  )
  args <- c(
    deparse_string(t[["name"]]),
    paste0("fields = ", render_fields(fields, ns, "  ")),
    if (nzchar(t[["description"]] %||% "")) {
      paste0("description = ", deparse_string(t[["description"]]))
    },
    paste0("version = ", t[["version"]], "L"),
    if (length(t[["rules"]]) > 0L) {
      paste0("rules = ", render_rules(t[["rules"]], "  "))
    },
    if (rust_stored) "storage = \"rust\""
  )
  c(
    roxygen_block(t, qname),
    params,
    paste0("#' @return A `", qname, "` value."),
    "#' @export",
    paste0(t[["name"]], " <- R8::new_struct("),
    paste0("  ", args, c(rep(",", length(args) - 1L), "")),
    ")"
  )
} # /R8::render_struct


# %% render_enum ----
#' The lines of an enum's declaration
#'
#' @param t List: The IR declaration.
#' @param ns Character: The namespace.
#' @param rust_stored Logical: The type has a codec.
#'
#' @return Character: The lines.
#'
#' @keywords internal
#' @noRd
render_enum <- function(t, ns, rust_stored = FALSE) {
  qname <- paste0(ns, "::", t[["name"]])
  check_renderable(
    t,
    c("migrations", "aliases", "extensions"),
    paste0("`", qname, "`")
  )
  variants <- vapply(
    t[["variants"]],
    function(v) {
      args <- switch(
        v[["shape"]],
        unit = character(),
        struct = c(
          paste0("fields = ", render_fields(v[["fields"]], ns, "      ")),
          if (length(v[["rules"]]) > 0L) {
            paste0("rules = ", render_rules(v[["rules"]], "      "))
          }
        ),
        newtype = {
          # The core's declaration check resolves a newtype's struct within
          # the document's namespace only.
          if (!startsWith(v[["of"]], paste0(ns, "::"))) {
            r8_abort(
              "The variant `",
              v[["name"]],
              "` of `",
              qname,
              "` holds `",
              v[["of"]],
              "`, a struct of another namespace; a newtype variant holds a struct of its enum's namespace.",
              class = "r8_declaration_error"
            )
          }
          paste0("of = ", type_ref(v[["of"]], ns))
        }
      )
      if (nzchar(v[["description"]] %||% "")) {
        args <- c(
          args,
          paste0("description = ", deparse_string(v[["description"]]))
        )
      }
      paste0(
        backtick(v[["name"]]),
        " = R8::variant(",
        paste(args, collapse = ", "),
        ")"
      )
    },
    character(1)
  )
  args <- c(
    deparse_string(t[["name"]]),
    paste0(
      "variants = list(\n    ",
      paste(variants, collapse = ",\n    "),
      "\n  )"
    ),
    if (nzchar(t[["description"]] %||% "")) {
      paste0("description = ", deparse_string(t[["description"]]))
    },
    paste0("tag = ", deparse_string(t[["tag"]] %||% "type")),
    paste0("version = ", t[["version"]], "L"),
    if (rust_stored) "storage = \"rust\""
  )
  c(
    roxygen_block(t, qname),
    paste0("#' @return The type object of `", qname, "`."),
    "#' @export",
    paste0(t[["name"]], " <- R8::new_enum("),
    paste0("  ", args, c(rep(",", length(args) - 1L), "")),
    ")"
  )
} # /R8::render_enum


# %% roxygen_block ----
#' The title and description lines of a type's roxygen block
#'
#' @param t List: The IR declaration.
#' @param qname Character: The qualified name.
#'
#' @return Character: The lines.
#'
#' @keywords internal
#' @noRd
roxygen_block <- function(t, qname) {
  d <- t[["description"]] %||% ""
  c(
    paste0("#' ", t[["name"]]),
    "#'",
    if (nzchar(d)) {
      c(
        paste0("#' ", strsplit(roxygen_text(d), "\n", fixed = TRUE)[[1L]]),
        "#'"
      )
    },
    paste0("#' The ", t[["kind"]], " `", qname, "`, declared in Rust."),
    "#'"
  )
} # /R8::roxygen_block


# %% roxygen_text ----
#' Text for a roxygen comment, with Rd's `%` escaped
#'
#' @param x Character: The text.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
roxygen_text <- function(x) {
  gsub("%", "\\%", x, fixed = TRUE)
} # /R8::roxygen_text


# %% render_fields ----
#' The R list of fields
#'
#' @param fields List: The IR fields.
#' @param ns Character: The namespace.
#' @param indent Character: The indentation of the enclosing call's arguments.
#'
#' @return Character: One string, possibly over several lines.
#'
#' @keywords internal
#' @noRd
render_fields <- function(fields, ns, indent) {
  if (length(fields) == 0L) {
    return("list()")
  }
  items <- vapply(
    fields,
    function(f) paste0(backtick(f[["name"]]), " = ", render_field(f, ns)),
    character(1)
  )
  paste0(
    "list(\n",
    paste0(indent, "  ", items, collapse = ",\n"),
    "\n",
    indent,
    ")"
  )
} # /R8::render_fields


# %% render_field ----
#' A field as `field(...)`, or as its bare type when it has no other facts
#'
#' @param f List: The IR field.
#' @param ns Character: The namespace.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
render_field <- function(f, ns) {
  check_renderable(f, "inherited_from", paste0("The field `", f[["name"]], "`"))
  type <- render_type(f[["type"]], ns)
  args <- c(
    if (!is.null(f[["default"]])) {
      paste0("default = ", render_default(f[["default"]], f[["type"]], ns))
    },
    if (nzchar(f[["description"]] %||% "")) {
      paste0("description = ", deparse_string(f[["description"]]))
    },
    if (isTRUE(f[["sensitive"]])) "sensitive = TRUE",
    if (!is.null(f[["unit"]])) paste0("unit = ", deparse_string(f[["unit"]])),
    switch(
      f[["resolve"]][["policy"]] %||% "",
      computed = paste0("compute = ", render_logic(f[["resolve"]][["expr"]])),
      context = "context = TRUE"
    ),
    if (isTRUE(f[["unique"]])) "unique = TRUE",
    if (isFALSE(f[["required"]])) "required = FALSE",
    if (length(f[["extensions"]]) > 0L) {
      paste0("extensions = ", deparse_value(f[["extensions"]]))
    }
  )
  if (length(args) == 0L) {
    return(type)
  }
  # A column's own facts take column(); everything else is field().
  fn <- if (isTRUE(f[["unique"]]) || isFALSE(f[["required"]])) {
    "column"
  } else {
    "field"
  }
  paste0("R8::", fn, "(", paste(c(type, args), collapse = ", "), ")")
} # /R8::render_field


# %% render_type ----
#' A field type as the R call that declares it
#'
#' @param t List: The IR field type.
#' @param ns Character: The namespace.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
render_type <- function(t, ns) {
  kind <- t[["kind"]]
  call <- function(fn, ...) {
    args <- c(...)
    paste0("R8::", fn, "(", paste(args, collapse = ", "), ")")
  }
  arg <- function(name, value) {
    if (is.null(value)) NULL else paste0(name, " = ", value)
  }
  int <- function(x) if (is.null(x)) NULL else r_number(x, integer = TRUE)
  dbl <- function(x) if (is.null(x)) NULL else r_number(x)
  ints <- function(x) {
    if (is.null(x)) NULL else r_vector(vapply(x, r_number, "", integer = TRUE))
  }
  bounds <- function(f) {
    c(
      arg("min", f(t[["min"]])),
      arg("max", f(t[["max"]])),
      arg("exclusive_min", f(t[["exclusive_min"]])),
      arg("exclusive_max", f(t[["exclusive_max"]]))
    )
  }
  strings <- function(x) {
    if (is.null(x)) NULL else r_vector(vapply(x, deparse_string, ""))
  }
  switch(
    kind,
    logical = call("type_logical"),
    raw = call("type_raw"),
    date = call("type_date"),
    posixct = call("type_posixct"),
    json = call("type_json"),
    integer = call(
      "type_integer",
      bounds(int),
      arg("values", ints(t[["enum"]]))
    ),
    double = call(
      "type_double",
      bounds(dbl),
      if (isFALSE(t[["finite"]])) "finite = FALSE",
      arg(
        "values",
        if (!is.null(t[["enum"]])) r_vector(vapply(t[["enum"]], r_number, ""))
      )
    ),
    character = call(
      "type_character",
      arg(
        "pattern",
        if (!is.null(t[["pattern"]])) deparse_string(t[["pattern"]])
      ),
      arg("values", strings(t[["enum"]]))
    ),
    factor = call(
      "type_factor",
      arg("levels", strings(t[["levels"]])),
      if (isTRUE(t[["ordered"]])) "ordered = TRUE"
    ),
    r = call("type_r", deparse_string(t[["class"]])),
    vec = call(
      "vec",
      render_type(t[["element"]], ns),
      arg("min_length", int(t[["min_length"]])),
      arg("max_length", int(t[["max_length"]])),
      if (isTRUE(t[["unique"]])) "unique = TRUE"
    ),
    option = call("option", render_type(t[["inner"]], ns)),
    btree_map = ,
    index_map = call(
      kind,
      render_type(t[["value"]], ns),
      if (length(t[["keys"]]) > 0L) {
        arg(
          "keys",
          render_type(c(list(kind = "character"), t[["keys"]]), ns)
        )
      }
    ),
    named = type_ref(t[["name"]], ns),
    matrix = call(
      "type_matrix",
      render_type(t[["element"]], ns),
      arg("nrow", int(t[["nrow"]])),
      arg("ncol", int(t[["ncol"]]))
    ),
    matrix_of = call("type_matrix", arg("of", type_ref(t[["of"]], ns))),
    array = call(
      "type_array",
      render_type(t[["element"]], ns),
      arg("dim", ints(t[["dim"]]))
    ),
    sparse_matrix = call(
      "type_sparse_matrix",
      render_type(t[["element"]], ns),
      arg("nrow", int(t[["nrow"]])),
      arg("ncol", int(t[["ncol"]]))
    ),
    table = call(
      "type_table",
      paste0(
        "list(",
        paste(
          vapply(
            t[["columns"]],
            function(f) {
              paste0(backtick(f[["name"]]), " = ", render_field(f, ns))
            },
            ""
          ),
          collapse = ", "
        ),
        ")"
      ),
      if (isTRUE(t[["open"]])) "open = TRUE",
      if (length(t[["rules"]]) > 0L) {
        arg("rules", render_rules(t[["rules"]], ""))
      }
    ),
    table_of = call("type_table", arg("of", type_ref(t[["of"]], ns))),
    r8_abort(
      "The field type `",
      kind,
      "` has no R declaration call.",
      class = "r8_declaration_error"
    )
  )
} # /R8::render_type


# %% type_ref ----
#' The R object of a named type
#'
#' A type of the namespace `ns` is the object the rendered file defines; a
#' type of another namespace is the exported object that `foreign_refs()`
#' found, such as `r8resample::Resampler`.
#'
#' @param qname Character: The qualified name.
#' @param ns Character: The namespace.
#'
#' @return Character: The object as R source.
#'
#' @keywords internal
#' @noRd
type_ref <- function(qname, ns) {
  prefix <- paste0(ns, "::")
  if (startsWith(qname, prefix)) {
    return(backtick(substring(qname, nchar(prefix) + 1L)))
  }
  foreign <- .r8_render_refs[["map"]]
  if (qname %in% names(foreign)) {
    return(foreign[[qname]])
  }
  r8_abort(
    "The type `",
    qname,
    "` belongs to another namespace, whose package r8_document_rust() ",
    "finds among the Imports of the package's DESCRIPTION.",
    class = "r8_declaration_error"
  )
} # /R8::type_ref


# %% render_rules ----
#' The R list of rules
#'
#' @param rules List: The IR rules.
#' @param indent Character: The indentation of the enclosing call's arguments.
#'
#' @return Character: One string, possibly over several lines.
#'
#' @keywords internal
#' @noRd
render_rules <- function(rules, indent) {
  items <- vapply(
    rules,
    function(r) {
      paste0(
        "R8::rule(",
        paste(
          c(
            render_logic(r[["invariant"]]),
            if (!is.null(r[["when"]])) {
              paste0("when = ", render_logic(r[["when"]]))
            },
            if (!is.null(r[["message"]])) {
              paste0("message = ", deparse_string(r[["message"]]))
            },
            if (!is.null(r[["id"]])) {
              paste0("id = ", deparse_string(r[["id"]]))
            }
          ),
          collapse = ", "
        ),
        ")"
      )
    },
    character(1)
  )
  if (!nzchar(indent)) {
    return(paste0("list(", paste(items, collapse = ", "), ")"))
  }
  paste0(
    "list(\n",
    paste0(indent, "  ", items, collapse = ",\n"),
    "\n",
    indent,
    ")"
  )
} # /R8::render_rules


# %% render_logic ----
#' A JSONLogic expression as the R expression that compiles to it
#'
#' Renders the expression as R, compiles the rendering as `rule()` does, and
#' keeps it when the canonical JSON of both is the same; otherwise the
#' expression renders as `R8::json_ir()` of its JSON text, whose numbers keep
#' the spelling of the IR (canonical JSON writes every number as a double).
#'
#' @param x The JSONLogic expression, as `r8_exports_document()` returns it.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
render_logic <- function(x) {
  target <- canonical_ir(x)
  e <- logic_to_r(x)
  if (!is.null(e)) {
    text <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
    compiled <- r8_logic(parse(text = text, keep.source = FALSE)[[1L]])
    if (!is.character(compiled) && identical(canonical_ir(compiled), target)) {
      return(text)
    }
  }
  render_json_ir(r8_ir_json(x))
} # /R8::render_logic


# %% logic_to_r ----
#' A JSONLogic expression as an R call, or `NULL` when R has no form for it
#'
#' @param x The JSONLogic expression.
#'
#' @return Language, a constant, or `NULL`.
#'
#' @keywords internal
#' @noRd
logic_to_r <- function(x) {
  fail <- function() stop("no R form", call. = FALSE)
  to_r <- function(x) {
    if (inherits(x, "r8_int64")) {
      return(as.call(list(
        call("::", as.name("R8"), as.name("int64")),
        unclass(x)
      )))
    }
    if (inherits(x, "r8_json_ir")) {
      fail()
    }
    if (is.atomic(x) && length(x) == 1L) {
      return(x)
    }
    if (!is.list(x)) {
      fail()
    }
    keys <- names(x)
    if (is.null(keys)) {
      # An array literal of constants.
      if (
        all(vapply(x, function(a) is.atomic(a) && length(a) == 1L, logical(1)))
      ) {
        return(as.call(c(as.name("c"), x)))
      }
      fail()
    }
    if (length(x) != 1L) {
      fail()
    }
    op <- keys[[1L]]
    operands <- x[[1L]]
    if (op == "var") {
      path <- if (is.list(operands)) operands else list(operands)
      if (
        length(path) != 1L || !is.character(path[[1L]]) || !nzchar(path[[1L]])
      ) {
        fail()
      }
      # `.x` is the element of an enclosing all() or any().
      text <- path[[1L]]
      element <- startsWith(text, ".x")
      parts <- strsplit(sub("^\\.x\\.?", "", text), ".", fixed = TRUE)[[1L]]
      if (element) {
        parts <- c(".x", parts)
      }
      out <- as.name(parts[[1L]])
      for (p in parts[-1L]) {
        out <- call("$", out, as.name(p))
      }
      return(out)
    }
    if (!is.list(operands) || !is.null(names(operands))) {
      operands <- list(operands)
    }
    if (op == "is_variant") {
      if (length(operands) != 2L || !is.character(operands[[2L]])) {
        fail()
      }
      return(call("is_variant", to_r(operands[[1L]]), operands[[2L]]))
    }
    if (op == "if") {
      if (!length(operands) %in% c(2L, 3L)) {
        fail()
      }
      return(as.call(c(as.name("if"), lapply(operands, to_r))))
    }
    fn <- r_spelling()[op]
    if (is.na(fn)) {
      fail()
    }
    if (op %in% c("and", "or") && length(operands) != 2L) {
      fail()
    }
    as.call(c(as.name(unname(fn)), lapply(operands, to_r)))
  }
  tryCatch(to_r(x), error = function(e) NULL)
} # /R8::logic_to_r


# %% r_spelling ----
#' The R spelling of each JSONLogic operator that rules compile from
#'
#' @return Named character: R function names, by JSONLogic operator.
#'
#' @keywords internal
#' @noRd
r_spelling <- function() {
  ops <- r8_rule_operators()
  ops <- ops[!names(ops) %in% c("&", "|")]
  stats::setNames(names(ops), unname(ops))
} # /R8::r_spelling


# %% render_default ----
#' A default value as the R value that declares it
#'
#' Logical, integer, double, and character values, vectors of them, and the
#' unit variants of an enum of the namespace render as R values; the R value
#' of a scalar or a vector is kept when its JSON form as a default is the same
#' canonical JSON. Every other default renders as `R8::json_ir()`.
#'
#' @param x The default, as `r8_exports_document()` returns it.
#' @param type List: The field's IR type.
#' @param ns Character: The namespace.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
render_default <- function(x, type, ns) {
  target <- canonical_ir(x)
  base <- if (identical(type[["kind"]], "option")) type[["inner"]] else type
  kind <- base[["kind"]]
  scalar <- function(v, k) {
    if (inherits(v, "r8_json_ir") || !is.atomic(v) || length(v) != 1L) {
      return(NULL)
    }
    switch(
      k,
      logical = if (is.logical(v)) deparse(v),
      integer = if (is.numeric(v)) r_number(v, integer = TRUE),
      double = if (is.numeric(v)) r_number(v),
      character = if (is.character(v)) deparse_string(v),
      NULL
    )
  }
  text <- if (kind %in% c("logical", "integer", "double", "character")) {
    scalar(x, kind)
  } else if (identical(kind, "vec") && is.list(x) && length(x) > 0L) {
    items <- lapply(x, scalar, k = base[["element"]][["kind"]])
    if (all(!vapply(items, is.null, logical(1)))) {
      r_vector(unlist(items))
    }
  } else if (identical(kind, "named") && is.character(x) && length(x) == 1L) {
    # A unit variant of an enum, written as the variant's name; the
    # declaration of the file checks it.
    paste0(type_ref(base[["name"]], ns), "@", backtick(x))
  }
  if (!is.null(text) && !identical(kind, "named")) {
    value <- eval(parse(text = text, keep.source = FALSE)[[1L]], baseenv())
    if (!identical(r8_default_canonical(value, type), target)) {
      text <- NULL
    }
  }
  text %||% render_json_ir(r8_ir_json(x))
} # /R8::render_default


# %% canonical_ir ----
#' The canonical JSON text of an IR value
#'
#' @param x An IR value, as `r8_exports_document()` returns it or an R declaration
#'   builds it.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
canonical_ir <- function(x) {
  r8_canonical_json(r8_ir_json(x))
} # /R8::canonical_ir


# %% render_json_ir ----
#' `R8::json_ir()` of JSON text
#'
#' @param text Character: JSON text.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
render_json_ir <- function(text) {
  quoted <- if (!grepl("['\\\\]", text)) {
    paste0("'", text, "'")
  } else {
    deparse_string(text)
  }
  paste0("R8::json_ir(", quoted, ")")
} # /R8::render_json_ir


# %% r_number ----
#' A number as R source, in the fewest digits that read back exactly
#'
#' @param x Numeric: One number.
#' @param integer Logical: Write an integer literal.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
r_number <- function(x, integer = FALSE) {
  x <- as.numeric(unclass(x))
  if (integer) {
    return(paste0(format(as.integer(x)), "L"))
  }
  for (d in 15:17) {
    s <- sprintf("%.*g", d, x)
    if (as.numeric(s) == x) {
      break
    }
  }
  s
} # /R8::r_number


# %% r_vector ----
#' Values as R source of a vector
#'
#' @param x Character: The values as R source.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
r_vector <- function(x) {
  if (length(x) == 1L) x else paste0("c(", paste(x, collapse = ", "), ")")
} # /R8::r_vector


# %% deparse_string ----
#' A string as R source
#'
#' @param x Character: One string.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
deparse_string <- function(x) {
  encodeString(x, quote = "\"")
} # /R8::deparse_string


# %% deparse_value ----
#' A value as R source, on one line
#'
#' @param x The value.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
deparse_value <- function(x) {
  paste(deparse(x, width.cutoff = 500L), collapse = " ")
} # /R8::deparse_value


# %% backtick ----
#' A name as R source, backticked when it is not syntactic
#'
#' @param x Character: The name.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
backtick <- function(x) {
  if (make.names(x) == x) x else paste0("`", x, "`")
} # /R8::backtick
