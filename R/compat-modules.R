# Shiny Modules Compatibility
# Implements NS(), moduleServer() and callModule() on top of hotShiny's graph
# model. A module namespace becomes a prefix on input/output ids: an input
# `x` inside module `mod` is the top-level input `mod-x`, matching Shiny.

#' Create a namespace function or namespaced id
#'
#' @param namespace The module namespace string
#' @param id Optional id to namespace immediately
#' @return If `id` is supplied, the namespaced id string; otherwise a function
#'   that namespaces ids.
#' @export
NS <- function(namespace, id = NULL) {
  if (missing(id) || is.null(id)) {
    function(id) ns_concat(namespace, id)
  } else {
    ns_concat(namespace, id)
  }
}

#' Default namespace separator (matches Shiny's "-")
#' @keywords internal
ns.sep <- "-"

#' @keywords internal
ns_concat <- function(namespace, id) {
  if (is.null(namespace) || !nzchar(namespace)) return(id)
  if (is.null(id)) return(namespace)
  paste(namespace, id, sep = ns.sep)
}

# Build an InputProxy that transparently namespaces ids.
.namespaced_input <- function(parent_input, prefix) {
  proxy <- new.env(parent = emptyenv())
  proxy$get <- function(name) {
    full <- ns_concat(prefix, name)
    if (inherits(parent_input, "InputProxy")) {
      return(parent_input$get(full))
    }
    parent_input[[full]]
  }
  class(proxy) <- "InputProxy"
  proxy
}

# Build an OutputProxy that namespaces output ids when assigned.
.namespaced_output <- function(prefix) {
  out_env <- new.env(parent = emptyenv())
  class(out_env) <- "OutputProxy"
  attr(out_env, "builder") <- get_graph_builder()
  attr(out_env, "ns_prefix") <- prefix
  out_env
}

# After running a module, make sure any render assigned into its output proxy
# is registered with the namespaced output id.
.finalize_module_outputs <- function(out_env, prefix) {
  if (!is.environment(out_env)) return(invisible(NULL))
  for (name in ls(envir = out_env, all.names = TRUE)) {
    val <- get(name, envir = out_env, inherits = FALSE)
    if (inherits(val, "RenderProxy")) {
      target <- ns_concat(prefix, name)
      if (isTRUE(val$pending_output_name) || is.null(val$output_name) ||
          !identical(val$output_name, target)) {
        tryCatch(val$set_output_name(target), error = function(e) {
          warning("Failed to register module output '", target, "': ", conditionMessage(e))
        })
      }
    }
  }
  invisible(NULL)
}

#' Run a module server function within a namespace
#'
#' @param id The module instance id (namespace)
#' @param module A function `function(input, output, session)` (the module body)
#' @param session The parent session
#' @return The module function's return value (invisibly)
#' @export
moduleServer <- function(id, module, session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    # Module invoked outside a running session (e.g. during graph building):
    # synthesize a minimal session so ns() works.
    session <- ShinySession$new(ns_prefix = id)
  }
  child <- session$makeScope(id)
  prefix <- child$ns_prefix

  child_input <- .namespaced_input(session$input, prefix)
  child_output <- .namespaced_output(prefix)
  child$input <- child_input
  child$output <- child_output

  result <- module(child_input, child_output, child)
  .finalize_module_outputs(child_output, prefix)
  invisible(result)
}

#' Legacy module invocation (Shiny < 1.5 style)
#'
#' @param module A function `function(input, output, session, ...)`
#' @param id The module instance id (namespace)
#' @param ... Extra arguments passed to the module
#' @param session The parent session
#' @return The module's return value (invisibly)
#' @export
callModule <- function(module, id, ..., session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    session <- ShinySession$new(ns_prefix = id)
  }
  child <- session$makeScope(id)
  prefix <- child$ns_prefix

  child_input <- .namespaced_input(session$input, prefix)
  child_output <- .namespaced_output(prefix)
  child$input <- child_input
  child$output <- child_output

  result <- module(child_input, child_output, child, ...)
  .finalize_module_outputs(child_output, prefix)
  invisible(result)
}
