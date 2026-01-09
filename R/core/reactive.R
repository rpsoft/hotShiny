# Reactive Expression Implementation
# Reimplements reactive() to build graph instead of executing

#' Create a reactive expression
#'
#' @param x Expression to make reactive
#' @param label Optional label for debugging
#' @return Reactive proxy object
reactive <- function(x, label = NULL) {
  # Get the current graph builder from context
  builder <- get_graph_builder()

  if (is.null(builder)) {
    stop("reactive() must be called within a hotShiny app context")
  }

  # Capture the expression
  expr <- rlang::enquo(x)
  expr_ast <- rlang::get_expr(expr)

  # Extract dependencies
  deps <- extract_dependencies(expr_ast)

  # Get source location if available
  source <- get_source_location()

  # Register in graph
  node <- builder$register_reactive(
    expr = expr_ast,
    name = label,
    deps = deps,
    source = source,
    env = parent.frame()
  )

  # Create proxy object that behaves like Shiny's reactive
  ReactiveProxy$new(node_id = node$id, builder = builder)
}

#' Reactive Proxy
#'
#' Proxy object that mimics Shiny's reactive behavior
ReactiveProxy <- R6::R6Class("ReactiveProxy",
  public = list(
    node_id = NULL,
    builder = NULL,
    executor = NULL,
    initialize = function(node_id, builder) {
      self$node_id <- node_id
      self$builder <- builder
    },

    # Get the executor (lazy initialization)
    get_executor = function() {
      if (is.null(self$executor)) {
        self$executor <- get_executor()
      }
      self$executor
    },

    # Call reactive (like data())
    call = function() {
      executor <- self$get_executor()
      if (is.null(executor)) {
        stop("Reactive executor not available")
      }
      executor$get_value(self$node_id)
    }
  )
)

# Make ReactiveProxy callable
`$.ReactiveProxy` <- function(x, name) {
  if (name == "call") {
    return(x$call)
  }
  NextMethod()
}

# Make it work with () syntax
`[[.ReactiveProxy` <- function(x, i, ...) {
  if (i == 1L || identical(i, "call")) {
    return(x$call)
  }
  NextMethod()
}

# Track reactive variable names when they're assigned
# This allows extract_dependencies to find reactive expressions by name
# We'll intercept assignments to track reactive proxies
`$<-.ReactiveProxy` <- function(x, name, value) {
  # This shouldn't be called, but if it is, just assign
  NextMethod()
}

# When a reactive proxy is assigned to a variable, register the name
# This is a workaround - we intercept the assignment
# But actually, we can't easily do this in R
# Instead, we'll use a different approach: track by inspecting the environment

# Context management for graph builder
graph_builder_context <- new.env(parent = emptyenv())

#' Set the current graph builder
#'
#' @param builder GraphBuilder instance
set_graph_builder <- function(builder) {
  assign("current_builder", builder, envir = graph_builder_context)
}

#' Get the current graph builder
#'
#' @return GraphBuilder instance or NULL
get_graph_builder <- function() {
  get("current_builder", envir = graph_builder_context, inherits = FALSE)
}

#' Get source location from call stack
#'
#' @return List with file and line, or NULL
get_source_location <- function() {
  # Try to get source location from sys.call()
  calls <- sys.calls()
  if (length(calls) > 0) {
    # Look for srcref in the call
    # This is a simplified version - real implementation would be more robust
    return(list(file = "<unknown>", line = NA_integer_))
  }
  NULL
}
