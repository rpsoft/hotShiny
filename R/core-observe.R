# Observer Implementation
# Reimplements observe() and observeEvent() to build graph

#' Create an observer
#'
#' @param x Expression to observe
#' @param label Optional label
#' @param suspended Whether observer starts suspended
#' @param priority Observer priority
#' @param domain Domain (for compatibility)
#' @param autoDestroy Whether to auto-destroy
#' @param ... Additional arguments
observe <- function(x, label = NULL, suspended = FALSE, priority = 0L,
                    domain = NULL, autoDestroy = TRUE, ...) {
  builder <- get_graph_builder()

  if (is.null(builder)) {
    stop("observe() must be called within a hotShiny app context")
  }

  expr <- rlang::enquo(x)
  expr_ast <- rlang::get_expr(expr)

  deps <- extract_dependencies(expr_ast)
  source <- get_source_location()

  node <- builder$register_observer(
    expr = expr_ast,
    deps = deps,
    priority = priority,
    once = FALSE,
    suspended = suspended,
    source = source,
    env = parent.frame()
  )

  # Return observer object
  ObserverProxy$new(node_id = node$id, builder = builder)
}

#' Create an event-driven observer
#'
#' @param eventExpr Event expression
#' @param handlerExpr Handler expression
#' @param label Optional label
#' @param suspended Whether observer starts suspended
#' @param priority Observer priority
#' @param domain Domain (for compatibility)
#' @param once Whether to run only once
#' @param ignoreInit Whether to ignore initial execution
#' @param ignoreNULL Whether to ignore NULL values
#' @param ... Additional arguments
observeEvent <- function(eventExpr, handlerExpr, label = NULL, suspended = FALSE,
                         priority = 0L, domain = NULL, once = FALSE,
                         ignoreInit = FALSE, ignoreNULL = TRUE, ...) {
  builder <- get_graph_builder()

  if (is.null(builder)) {
    stop("observeEvent() must be called within a hotShiny app context")
  }

  # Extract event dependencies
  event_expr <- rlang::enquo(eventExpr)
  event_ast <- rlang::get_expr(event_expr)
  event_deps <- extract_dependencies(event_ast)

  # Handler expression
  handler_expr <- rlang::enquo(handlerExpr)
  handler_ast <- rlang::get_expr(handler_expr)
  handler_deps <- extract_dependencies(handler_ast)

  # Combine dependencies
  deps <- unique(c(event_deps, handler_deps))

  source <- get_source_location()

  # Create observer node with event flag
  node <- builder$register_observer(
    expr = handler_ast,
    deps = deps,
    priority = priority,
    once = once,
    suspended = suspended,
    source = source,
    env = parent.frame()
  )

  # Mark as event-driven
  node$metadata$event_driven <- TRUE
  node$metadata$event_expr <- expr_to_ast(event_ast)
  node$metadata$ignoreInit <- ignoreInit
  node$metadata$ignoreNULL <- ignoreNULL

  ObserverProxy$new(node_id = node$id, builder = builder)
}

#' Observer Proxy
#'
#' Proxy object for observers
ObserverProxy <- R6::R6Class("ObserverProxy",
  public = list(
    node_id = NULL,
    builder = NULL,
    initialize = function(node_id, builder) {
      self$node_id <- node_id
      self$builder <- builder
    },

    # Suspend the observer
    suspend = function() {
      node <- self$builder$get_graph()$get_node(self$node_id)
      if (!is.null(node) && inherits(node, "ObserverNode")) {
        node$suspended <- TRUE
        node$metadata$suspended <- TRUE
      }
    },

    # Resume the observer
    resume = function() {
      node <- self$builder$get_graph()$get_node(self$node_id)
      if (!is.null(node) && inherits(node, "ObserverNode")) {
        node$suspended <- FALSE
        node$metadata$suspended <- FALSE
      }
    },

    # Destroy the observer
    destroy = function() {
      # Remove from graph
      graph <- self$builder$get_graph()
      # Note: Full removal would require graph modification
      # For now, mark as destroyed
      node <- graph$get_node(self$node_id)
      if (!is.null(node)) {
        node$metadata$destroyed <- TRUE
      }
    }
  )
)
