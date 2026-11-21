# roclet.R
# ::R8::
# 2026- EDG rtemis.org

# The roxygen2 roclet `r8_rd`, which a package lists in place of `rd`. It adds
# to each `implement()` block the tags that place the method in an Rd file,
# and to each enum block a usage of its variants, then runs the `rd` roclet.
# spec: R8/class-system#r-familiar-layer

# %% r8_rd ----
#' Rd files for R8 declarations
#'
#' A roxygen2 roclet that writes the Rd files of the `rd` roclet and adds R8
#' methods and enum variants to them. A package lists it in place of `rd`:
#' `Roxygen: list(roclets = c("collate", "namespace", "R8::r8_rd"))` in its
#' `DESCRIPTION`.
#'
#' Each `implement(generic, Type) <- function(...)` call adds an entry to the
#' usage section of an Rd file, `## R8 method for struct 'Type'` (or `enum`)
#' followed by the call with the method's arguments. The entry goes to the Rd
#' file of the generic when the package declares and documents the generic,
#' an R8 generic or an S3 generic, and otherwise to the Rd file of the type. A roxygen block above the call
#' adds its `@param` entries to that file; a call without a block is found by
#' scanning `R/`, and is left out when neither the generic nor the type has an
#' Rd file in the package. A method the package implements in compiled code
#' (`R8.h`), registered when roxygen2 loads the package, takes an entry by the
#' same rules, with the formals of the trait's method. The usage of an enum lists its variants, such as
#' `Method@KFold(k)` and `Method@Bootstrap`.
#'
#' @return A roxygen2 roclet, of class `roclet_r8_rd`.
#'
#' @examples
#' \dontrun{
#' roxygen2::roxygenise(roclets = c("collate", "namespace", "R8::r8_rd"))
#' }
#'
#' @export
r8_rd <- function() {
  if (!requireNamespace("roxygen2", quietly = TRUE)) {
    r8_abort(
      "The roclet r8_rd needs the package 'roxygen2'.",
      class = "r8_declaration_error"
    )
  }
  roxygen2::roclet(c("r8_rd", "rd"))
} # /R8::r8_rd


# %% is_implement_call ----
#' A call `implement(generic, Type) <- fn`
#'
#' @param call A call.
#'
#' @return Logical.
#'
#' @keywords internal
#' @noRd
is_implement_call <- function(call) {
  is.call(call) &&
    (identical(call[[1L]], quote(`<-`)) || identical(call[[1L]], quote(`=`))) &&
    is.call(call[[2L]]) &&
    paste(deparse(call[[2L]][[1L]]), collapse = "") %in%
      c("implement", "R8::implement")
} # /R8::is_implement_call


# %% type_kind ----
#' The kind of an R8 type, as its usage entry names it
#'
#' @param type An R8 type object.
#'
#' @return Character: `"struct"` or `"enum"`.
#'
#' @keywords internal
#' @noRd
type_kind <- function(type) {
  if (inherits(type, "r8_enum_type")) "enum" else "struct"
} # /R8::type_kind


# %% roxy_tag_of ----
#' A roxygen2 tag at a block's location
#'
#' @param block A roxygen2 block.
#' @param tag Character: The tag name.
#' @param val The tag's value.
#'
#' @return A roxygen2 tag.
#'
#' @keywords internal
#' @noRd
roxy_tag_of <- function(block, tag, val) {
  roxygen2::roxy_tag(
    tag,
    raw = paste(as.character(val), collapse = " "),
    val = val,
    file = block$file,
    line = block$line
  )
} # /R8::roxy_tag_of


# %% call_text ----
#' The usage text of a call with a function's formals
#'
#' @param name Character: The function name.
#' @param formals List: The formals.
#'
#' @return Character.
#'
#' @keywords internal
#' @noRd
call_text <- function(name, formals) {
  args <- vapply(
    names(formals),
    function(n) {
      if (identical(formals[[n]], quote(expr = ))) {
        n
      } else {
        paste0(n, " = ", paste(deparse(formals[[n]]), collapse = ""))
      }
    },
    character(1)
  )
  paste0(name, "(", paste(args, collapse = ", "), ")")
} # /R8::call_text


# %% topic_of ----
#' The Rd file name of the block that documents an object
#'
#' @param blocks List: roxygen2 blocks.
#' @param value The object.
#'
#' @return Character, or `NULL` when no block documents `value`.
#'
#' @keywords internal
#' @noRd
topic_of <- function(blocks, value) {
  for (b in blocks) {
    if (
      !is.null(b$object) &&
        identical(roxygen_value(b$object$value), value) &&
        !roxygen2::block_has_tags(b, "noRd")
    ) {
      rdname <- roxygen2::block_get_tag_value(b, "rdname")
      if (!is.null(rdname)) {
        return(rdname)
      }
      name <- roxygen2::block_get_tag_value(b, "name")
      return(if (is.null(name)) b$object$alias else name)
    }
  }
  NULL
} # /R8::topic_of


# %% roxygen_value ----
#' A block's object value without the class roxygen2 adds
#'
#' roxygen2 8.1.1 adds the class `s3generic` to the value of an S3 generic's
#' block.
#'
#' @param x The value.
#'
#' @return `x` without that class.
#'
#' @keywords internal
#' @noRd
roxygen_value <- function(x) {
  if (inherits(x, "s3generic")) {
    cls <- setdiff(class(x), c("s3generic", "function"))
    class(x) <- if (length(cls) > 0L) c(cls, "function") else NULL
  }
  x
} # /R8::roxygen_value


# %% scan_unblocked ----
#' The `implement()` calls of a package without a roxygen block
#'
#' @param blocks List: roxygen2 blocks.
#' @param base_path Character: The package directory.
#'
#' @return List: An empty block per call.
#'
#' @keywords internal
#' @noRd
scan_unblocked <- function(blocks, base_path) {
  key <- function(file, call) {
    paste(basename(file), paste(deparse(call), collapse = ""))
  }
  seen <- vapply(blocks, function(b) key(b$file, b$call), character(1))
  out <- list()
  files <- list.files(
    file.path(base_path, "R"),
    pattern = "[.][Rr]$",
    full.names = TRUE
  )
  for (f in files) {
    exprs <- parse(f, keep.source = TRUE)
    lines <- utils::getSrcLocation(exprs, "line")
    for (i in seq_along(exprs)) {
      if (is_implement_call(exprs[[i]]) && !key(f, exprs[[i]]) %in% seen) {
        out <- c(
          out,
          list(roxygen2::roxy_block(
            list(),
            file = f,
            line = lines[[i]],
            call = exprs[[i]]
          ))
        )
      }
    }
  }
  out
} # /R8::scan_unblocked


# %% method_block ----
#' Add the tags of an R8 method to an `implement()` block
#'
#' @param block A roxygen2 block of an `implement()` call.
#' @param blocks List: Every block of the package.
#' @param env Environment: The package environment.
#'
#' @return The block with its tags, or `NULL` when it documents nothing.
#'
#' @keywords internal
#' @noRd
method_block <- function(block, blocks, env) {
  call <- block$call
  generic <- eval(call[[2L]][[2L]], env)
  type <- eval(call[[2L]][[3L]], env)
  fn <- eval(call[[3L]], env)
  gname <- paste(deparse(call[[2L]][[2L]]), collapse = "")
  tname <- sub("^.*::", "", attr(type, "r8_name"))
  target <- classify_generic(generic, tname)
  topic <- NULL
  if (identical(target[["kind"]], "trait")) {
    trait_ns <- sub("::.*", "", unclass(target[["trait"]])[["name"]])
    if (identical(trait_ns, declaring_namespace(env))) {
      topic <- topic_of(blocks, generic)
    }
  } else if (identical(target[["package"]], declaring_package(env))) {
    topic <- topic_of(blocks, generic)
  }
  if (is.null(topic)) {
    topic <- topic_of(blocks, type)
  }
  documented <- length(block$tags) > 0L
  if (is.null(topic)) {
    if (!documented) {
      return(NULL)
    }
    topic <- paste0(gname, "-", tname)
  }
  usage <- paste0(
    "## R8 method for ",
    type_kind(type),
    " '",
    tname,
    "'\n\\special{",
    call_text(gname, formals(fn)),
    "}"
  )
  new <- list(
    roxy_tag_of(block, "usage", structure(usage, class = "rd")),
    roxy_tag_of(block, ".formals", names(formals(fn)))
  )
  if (!roxygen2::block_has_tags(block, c("name", "rdname", "describeIn"))) {
    new <- c(
      new,
      list(
        roxy_tag_of(block, "name", topic),
        roxy_tag_of(block, "rdname", topic)
      )
    )
  } else if (!roxygen2::block_has_tags(block, "name")) {
    new <- c(
      new,
      list(roxy_tag_of(
        block,
        "name",
        roxygen2::block_get_tag_value(block, "rdname")
      ))
    )
  }
  block$tags <- c(block$tags, new)
  block
} # /R8::method_block


# %% compiled_blocks ----
#' Blocks of the methods a package implements in compiled code
#'
#' One block per compiled method that the package registered: its usage
#' entry goes to the Rd file of the generic when the trait belongs to the
#' package's namespace and the package documents the generic, and otherwise
#' to the Rd file of the type; a method neither has a file for is left out.
#'
#' @param blocks List: The package's roxygen2 blocks.
#' @param env Environment: The package's namespace.
#' @param base_path Character: The package's root directory.
#'
#' @return List: The blocks.
#'
#' @keywords internal
#' @noRd
compiled_blocks <- function(blocks, env, base_path) {
  methods <- .r8_compiled_methods[[declaring_package(env)]]
  if (is.null(methods)) {
    return(list())
  }
  ns <- declaring_namespace(env)
  out <- list()
  for (i in seq_len(nrow(methods))) {
    trait <- .r8_traits[[methods[["trait_name"]][[i]]]]
    type <- .r8_types[[methods[["type_name"]][[i]]]]
    if (is.null(trait) || is.null(type)) {
      next
    }
    t <- unclass(trait)
    method <- methods[["method"]][[i]]
    decl <- Filter(
      function(m) identical(m[["name"]], method),
      trait_ir(t)[["methods"]]
    )
    params <- unlist(decl[[1L]][["params"]])
    formals <- stats::setNames(
      rep(list(quote(expr = )), length(params)),
      params
    )
    topic <- NULL
    if (identical(sub("::.*", "", methods[["trait_name"]][[i]]), ns)) {
      topic <- topic_of(blocks, t[["generics"]][[method]])
    }
    if (is.null(topic)) {
      topic <- topic_of(blocks, type)
    }
    if (is.null(topic)) {
      next
    }
    tname <- sub("^.*::", "", methods[["type_name"]][[i]])
    block <- roxygen2::roxy_block(
      list(),
      file = file.path(base_path, "src"),
      line = 1L,
      call = NULL
    )
    usage <- paste0(
      "## R8 method for ",
      type_kind(type),
      " '",
      tname,
      "'\n\\special{",
      call_text(method, formals),
      "}"
    )
    block$tags <- list(
      roxy_tag_of(block, "usage", structure(usage, class = "rd")),
      roxy_tag_of(block, ".formals", params),
      roxy_tag_of(block, "name", topic),
      roxy_tag_of(block, "rdname", topic)
    )
    out <- c(out, list(block))
  }
  out
} # /R8::compiled_blocks


# %% enum_block ----
#' Add the usage of an enum's variants to its block
#'
#' Replaces the usage that roxygen2 generates for a function object, a tag
#' without raw text; a usage written in the block is kept.
#'
#' @param block A roxygen2 block of an enum.
#'
#' @return The block.
#'
#' @keywords internal
#' @noRd
enum_block <- function(block) {
  type <- block$object$value
  name <- block$object$alias
  variants <- type_ir(type)[["variants"]]
  lines <- vapply(
    variants,
    function(v) {
      item <- paste0(name, "@", v[["name"]])
      text <- switch(
        v[["shape"]],
        unit = item,
        struct = call_text(item, ctor_formals(v[["fields"]])),
        newtype = call_text(item, formals(.r8_types[[v[["of"]]]]))
      )
      paste0("\\special{", text, "}")
    },
    character(1)
  )
  block$tags <- c(
    Filter(function(t) !(t$tag == "usage" && is.null(t$raw)), block$tags),
    list(roxy_tag_of(
      block,
      "usage",
      structure(paste(lines, collapse = "\n"), class = "rd")
    ))
  )
  block
} # /R8::enum_block


# %% roclet_process.roclet_r8_rd ----
#' @exportS3Method roxygen2::roclet_process
roclet_process.roclet_r8_rd <- function(x, blocks, env, base_path) {
  blocks <- c(blocks, scan_unblocked(blocks, base_path))
  compiled <- compiled_blocks(blocks, env, base_path)
  out <- list()
  for (block in blocks) {
    if (is_implement_call(block$call)) {
      if (roxygen2::block_has_tags(block, "noRd")) {
        next
      }
      block <- method_block(block, blocks, env)
    } else if (
      inherits(block$object$value, "r8_enum_type") &&
        !roxygen2::block_has_tags(block, "noRd") &&
        !any(vapply(
          block$tags,
          function(t) t$tag == "usage" && !is.null(t$raw),
          logical(1)
        ))
    ) {
      block <- enum_block(block)
    }
    if (!is.null(block)) {
      out <- c(out, list(block))
    }
  }
  out <- c(out, compiled)
  roxygen2::roclet_process(roxygen2::rd_roclet(), out, env, base_path)
} # /R8::roclet_process.roclet_r8_rd


# %% roclet_output.roclet_r8_rd ----
#' @exportS3Method roxygen2::roclet_output
roclet_output.roclet_r8_rd <- function(x, results, base_path, ...) {
  roxygen2::roclet_output(roxygen2::rd_roclet(), results, base_path, ...)
} # /R8::roclet_output.roclet_r8_rd


# %% roclet_clean.roclet_r8_rd ----
#' @exportS3Method roxygen2::roclet_clean
roclet_clean.roclet_r8_rd <- function(x, base_path) {
  roxygen2::roclet_clean(roxygen2::rd_roclet(), base_path)
} # /R8::roclet_clean.roclet_r8_rd
