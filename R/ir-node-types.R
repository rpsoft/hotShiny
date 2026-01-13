# IR Node System
# Defines the intermediate representation for reactive nodes

#' Base class for all reactive nodes
#'
#' @param id Unique identifier for the node
#' @param type Node type (reactive, input, output, observe, render)
#' @param deps List of dependency node IDs
#' @param expr Expression AST (not closure)
#' @param version Version number for hot reload
#' @param source Source location (file, line)
#' @param metadata Additional metadata
ReactiveNode <- R6::R6Class("ReactiveNode",
  public = list(
    id = NULL,
    type = NULL,
    deps = NULL,
    expr = NULL,
    version = NULL,
    source = NULL,
    env = NULL,
    metadata = NULL,
    initialize = function(id, type, deps = list(), expr = NULL,
                          version = 1L, source = NULL, metadata = list(), env = NULL) {
      self$id <- id
      self$type <- type
      self$deps <- deps
      self$expr <- expr
      self$version <- version
      self$source <- source
      self$metadata <- metadata
      self$env <- env
    },

    # Serialize node to list (for JSON)
    to_list = function() {
      list(
        id = self$id,
        type = self$type,
        deps = self$deps,
        expr = self$expr,
        version = self$version,
        source = self$source,
        metadata = self$metadata
      )
    },

    # Compute hash for node content (for diffing)
    compute_hash = function() {
      content <- list(
        type = self$type,
        deps = sort(unlist(self$deps)),
        expr = self$expr
      )
      digest::digest(content, algo = "sha256")
    }
  )
)

#' Input node - represents input$x
InputNode <- R6::R6Class("InputNode",
  inherit = ReactiveNode,
  public = list(
    input_name = NULL,
    initialize = function(id, input_name, version = 1L, source = NULL, env = NULL) {
      super$initialize(
        id = id,
        type = "input",
        deps = list(),
        version = version,
        source = source,
        env = env
      )
      self$input_name <- input_name
      self$metadata$input_name <- input_name
    }
  )
)

#' Output node - represents output$x
OutputNode <- R6::R6Class("OutputNode",
  inherit = ReactiveNode,
  public = list(
    output_name = NULL,
    render_type = NULL,
    initialize = function(id, output_name, render_type = NULL,
                          deps = list(), expr = NULL, version = 1L, source = NULL, env = NULL) {
      super$initialize(
        id = id,
        type = "output",
        deps = deps,
        expr = expr,
        version = version,
        source = source,
        env = env
      )
      self$output_name <- output_name
      self$render_type <- render_type
      self$metadata$output_name <- output_name
      self$metadata$render_type <- render_type
    }
  )
)

#' Reactive expression node - represents reactive({ ... })
ReactiveExprNode <- R6::R6Class("ReactiveExprNode",
  inherit = ReactiveNode,
  public = list(
    name = NULL,
    initialize = function(id, deps = list(), expr = NULL, name = NULL,
                          version = 1L, source = NULL, env = NULL) {
      super$initialize(
        id = id,
        type = "reactive",
        deps = deps,
        expr = expr,
        version = version,
        source = source,
        env = env
      )
      self$name <- name
      if (!is.null(name)) {
        self$metadata$name <- name
      }
    }
  )
)

#' Observer node - represents observe({ ... })
ObserverNode <- R6::R6Class("ObserverNode",
  inherit = ReactiveNode,
  public = list(
    priority = NULL,
    once = NULL,
    suspended = NULL,
    initialize = function(id, deps = list(), expr = NULL, priority = 0L,
                          once = FALSE, suspended = FALSE, version = 1L, source = NULL, env = NULL) {
      super$initialize(
        id = id,
        type = "observe",
        deps = deps,
        expr = expr,
        version = version,
        source = source,
        env = env
      )
      self$priority <- priority
      self$once <- once
      self$suspended <- suspended
      self$metadata$priority <- priority
      self$metadata$once <- once
      self$metadata$suspended <- suspended
    }
  )
)

#' Render node - represents render*() functions
RenderNode <- R6::R6Class("RenderNode",
  inherit = ReactiveNode,
  public = list(
    render_type = NULL,
    output_name = NULL,
    initialize = function(id, render_type, output_name, deps = list(),
                          expr = NULL, version = 1L, source = NULL, env = NULL, outputArgs = NULL) {
      super$initialize(
        id = id,
        type = "render",
        deps = deps,
        expr = expr,
        version = version,
        source = source,
        env = env
      )
      self$render_type <- render_type
      self$output_name <- output_name
      self$metadata$render_type <- render_type
      self$metadata$output_name <- output_name
      if (!is.null(outputArgs)) {
        self$metadata$outputArgs <- outputArgs
      }
    }
  )
)

#' Create a new node ID
#' @param prefix Prefix for the ID
#' @param counter Counter for uniqueness
new_node_id <- function(prefix = "node", counter = NULL) {
  if (is.null(counter)) {
    counter <- getOption("hotshiny.node.counter", 0L)
    options(hotshiny.node.counter = counter + 1L)
  }
  paste0(prefix, "_", counter)
}

#' Reset node ID counter (for testing)
reset_node_counter <- function() {
  options(hotshiny.node.counter = 0L)
}
