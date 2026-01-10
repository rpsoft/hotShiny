# Render Functions Implementation
# Reimplements render*() functions to build graph

#' Render text output
#'
#' @param expr Expression to render
#' @param env Environment
#' @param quoted Whether expression is quoted
#' @param outputArgs Output arguments
#' @param output_name Optional output name (usually set automatically)
#' @param ... Additional arguments
renderText <- function(expr, env = parent.frame(), quoted = FALSE,
                       outputArgs = list(), output_name = NULL, ...) {
  # If output_name not provided, try to get from context
  if (is.null(output_name)) {
    output_name <- get_current_output_name()
  }
  render_impl("text", expr, env, quoted, outputArgs, output_name = output_name, ...)
}

#' Render plot output
#'
#' @param expr Expression to render
#' @param env Environment
#' @param quoted Whether expression is quoted
#' @param width Width
#' @param height Height
#' @param ... Additional arguments
renderPlot <- function(expr, env = parent.frame(), quoted = FALSE,
                       width = "auto", height = "auto", ...) {
  render_impl("plot", expr, env, quoted, list(width = width, height = height), ...)
}

#' Render table output
#'
#' @param expr Expression to render
#' @param env Environment
#' @param quoted Whether expression is quoted
#' @param ... Additional arguments
renderTable <- function(expr, env = parent.frame(), quoted = FALSE, ...) {
  render_impl("table", expr, env, quoted, list(), ...)
}

#' Render data table output
#'
#' @param expr Expression to render
#' @param env Environment
#' @param quoted Whether expression is quoted
#' @param ... Additional arguments
renderDataTable <- function(expr, env = parent.frame(), quoted = FALSE, ...) {
  render_impl("datatable", expr, env, quoted, list(), ...)
}

#' Render UI output
#'
#' @param expr Expression to render
#' @param env Environment
#' @param quoted Whether expression is quoted
#' @param ... Additional arguments
renderUI <- function(expr, env = parent.frame(), quoted = FALSE, ...) {
  render_impl("ui", expr, env, quoted, list(), ...)
}

#' Generic render implementation
#'
#' @param render_type Type of render function
#' @param expr Expression to render
#' @param env Environment
#' @param quoted Whether expression is quoted
#' @param outputArgs Output arguments
#' @param ... Additional arguments
render_impl <- function(render_type, expr, env, quoted, outputArgs, output_name = NULL, ...) {
  builder <- get_graph_builder()

  if (is.null(builder)) {
    stop("render functions must be called within a hotShiny app context")
  }

  # Get the expression
  if (!quoted) {
    # Try multiple methods to capture the expression
    # Method 1: Use sys.call() to get the actual call from parent frame
    parent_call <- sys.call(-1)
    if (!is.null(parent_call) && length(parent_call) >= 2) {
      # Extract the expr argument (usually the second argument)
      expr_ast <- parent_call[[2]]
      # Check if it's actually an expression (not just the symbol 'expr')
      if (!rlang::is_symbol(expr_ast) || rlang::as_string(expr_ast) != "expr") {
        # Good, we got the actual expression
      } else {
        # Method 2: Fallback to enquo
        expr_quosure <- rlang::enquo(expr)
        expr_ast <- rlang::get_expr(expr_quosure)
      }
    } else {
      # Method 2: Fallback to enquo
      expr_quosure <- rlang::enquo(expr)
      expr_ast <- rlang::get_expr(expr_quosure)
    }
  } else {
    expr_ast <- expr
  }

  # Extract dependencies
  deps <- extract_dependencies(expr_ast)

  # Get output name from parameter or context
  # The issue is that when output$x <- renderText(...) is called,
  # R evaluates renderText(...) FIRST, then calls $<-
  # So we need to detect the assignment from the call stack
  if (is.null(output_name)) {
    output_name <- get_current_output_name()
  }

  # If still null, try to detect from call stack by looking ahead
  # We need to find the $<- call that will happen after renderText returns
  if (is.null(output_name)) {
    # Look in parent frames for the assignment
    # The assignment happens in a parent frame
    for (frame_num in 1:sys.nframe()) {
      frame_env <- sys.frame(frame_num)
      # Check if this frame has output$x <- ... pattern
      # This is tricky - we can't easily detect future assignments
      # So we'll use a workaround: store a pending assignment
    }

    # Alternative: Use a delayed registration approach
    # Return a proxy that registers when assigned
  }

  # If we still don't have output_name, we can't proceed
  # But for now, let's allow it to be set later via the $<- method
  # We'll store it in the RenderProxy

  source <- get_source_location()

  # If output_name is NULL, we'll set it later via the $<- method
  # For now, use a placeholder
  if (is.null(output_name)) {
    # Don't register yet - return a proxy that will register when assigned
    # Create a temporary proxy
    temp_id <- paste0("render_", as.character(Sys.time()))
    proxy <- RenderProxy$new(node_id = temp_id, render_type = render_type, builder = builder)
    proxy$output_name <- NULL
    proxy$pending_output_name <- TRUE
    proxy$pending_expr <- expr_ast
    proxy$pending_deps <- deps
    proxy$pending_source <- source
    proxy$pending_env <- env # Store environment
    # Store outputArgs if provided
    if (!is.null(outputArgs) && length(outputArgs) > 0) {
      proxy$pending_outputArgs <- outputArgs
    }
    return(proxy)
  }

  # Register render node
  node <- builder$register_render(
    render_type = render_type,
    output_name = output_name,
    expr = expr_ast,
    deps = deps,
    source = source,
    env = env,
    outputArgs = outputArgs
  )

  # Return render proxy
  proxy <- RenderProxy$new(node_id = node$id, render_type = render_type, builder = builder)
  proxy$output_name <- output_name
  proxy$pending_output_name <- FALSE
  proxy
}

#' Render Proxy
#'
#' Proxy object for render functions
RenderProxy <- R6::R6Class("RenderProxy",
  public = list(
    node_id = NULL,
    render_type = NULL,
    builder = NULL,
    output_name = NULL,
    pending_output_name = NULL,
    pending_expr = NULL,
    pending_deps = NULL,
    pending_source = NULL,
    pending_env = NULL,
    pending_outputArgs = NULL,
    initialize = function(node_id, render_type, builder) {
      self$node_id <- node_id
      self$render_type <- render_type
      self$builder <- builder
      self$output_name <- NULL
      self$pending_output_name <- FALSE
      self$pending_expr <- NULL
      self$pending_deps <- NULL
      self$pending_source <- NULL
      self$pending_env <- NULL
      self$pending_outputArgs <- NULL

      # If builder is NULL, try to get it from context
      if (is.null(self$builder)) {
        if (exists("get_graph_builder", envir = parent.frame())) {
          self$builder <- get("get_graph_builder", envir = parent.frame())()
        } else if (exists("get_graph_builder", envir = asNamespace("hotShiny"))) {
          self$builder <- get("get_graph_builder", envir = asNamespace("hotShiny"))()
        }
      }
    },

    # Set output name (called by $<- method)
    set_output_name = function(name) {
      self$output_name <- name

      # Ensure builder is available
      if (is.null(self$builder)) {
        if (exists("get_graph_builder", envir = parent.frame())) {
          self$builder <- get("get_graph_builder", envir = parent.frame())()
        } else if (exists("get_graph_builder", envir = asNamespace("hotShiny"))) {
          self$builder <- get("get_graph_builder", envir = asNamespace("hotShiny"))()
        }
      }

      if (is.null(self$builder)) {
        warning("Builder not available when setting output name for ", name)
        return(invisible(NULL))
      }

      # If this was a pending registration, do it now
      if (self$pending_output_name && !is.null(self$pending_expr)) {
        # Register the render node now that we have the output name
        tryCatch(
          {
            # Get outputArgs if they were stored in the proxy
            outputArgs <- self$pending_outputArgs
            node <- self$builder$register_render(
              render_type = self$render_type,
              output_name = name,
              expr = self$pending_expr,
              deps = self$pending_deps,
              source = self$pending_source,
              env = self$pending_env,
              outputArgs = outputArgs
            )
            self$node_id <- node$id
            self$pending_output_name <- FALSE
            self$pending_expr <- NULL
            self$pending_deps <- NULL
            self$pending_source <- NULL
            self$pending_env <- NULL
            log_debug("[RenderProxy] Successfully registered node: ", node$id, ", output_name=", node$output_name, "\n", file = stderr())
          },
          error = function(e) {
            warning("Error registering render node for ", name, ": ", conditionMessage(e))
          }
        )
      } else {
        # Update existing node
        if (!is.null(self$node_id) && !is.null(self$builder)) {
          node <- self$builder$get_graph()$get_node(self$node_id)
          if (!is.null(node) && inherits(node, "RenderNode")) {
            node$output_name <- name
            node$metadata$output_name <- name
          }
        }
      }
    }
  )
)

# ============================================================================
# Additional Render Functions
# ============================================================================

#' Render print output
#'
#' Captures printed output from an expression.
#'
#' @param expr Expression to evaluate and capture print output
#' @param env Environment for evaluation
#' @param quoted Whether expression is quoted
#' @param width Print width
#' @param outputArgs Output arguments
#' @param ... Additional arguments
#' @export
renderPrint <- function(expr, env = parent.frame(), quoted = FALSE,
                        width = getOption("width"), outputArgs = list(), ...) {
  output_name <- get_current_output_name()
  
  # Store width in outputArgs
  outputArgs$width <- width
  
  render_impl("print", expr, env, quoted, outputArgs, output_name = output_name, ...)
}

#' Render image output
#'
#' Renders an image from a file.
#'
#' @param expr Expression that returns a list with 'src' (file path) and optionally
#'   'contentType', 'width', 'height', 'alt'
#' @param env Environment for evaluation
#' @param quoted Whether expression is quoted
#' @param deleteFile Delete file after sending?
#' @param outputArgs Output arguments
#' @param ... Additional arguments
#' @export
renderImage <- function(expr, env = parent.frame(), quoted = FALSE,
                        deleteFile = TRUE, outputArgs = list(), ...) {
  output_name <- get_current_output_name()
  
  outputArgs$deleteFile <- deleteFile
  
  render_impl("image", expr, env, quoted, outputArgs, output_name = output_name, ...)
}

#' Create a download handler
#'
#' Creates a handler for file downloads.
#'
#' @param filename Function or string for filename
#' @param content Function that writes content to a file
#' @param contentType MIME type (or function returning MIME type)
#' @param outputArgs Output arguments
#' @return Download handler object
#' @export
downloadHandler <- function(filename, content, contentType = NA, outputArgs = list()) {
  handler <- list(
    filename = filename,
    content = content,
    contentType = contentType,
    outputArgs = outputArgs
  )
  class(handler) <- c("shiny.downloadHandler", "list")
  handler
}

#' Render cached plot output
#'
#' Like renderPlot but with caching support.
#'
#' @param expr Expression to render the plot
#' @param cacheKeyExpr Expression that returns the cache key
#' @param sizePolicy Size policy function
#' @param res Resolution
#' @param cache Cache object
#' @param ... Additional arguments passed to renderPlot
#' @export
renderCachedPlot <- function(expr, cacheKeyExpr, sizePolicy = sizeGrowthRatio(width = 400, height = 400, growthRate = 1.2),
                             res = 72, cache = "app", ...) {
  # For now, just delegate to renderPlot without caching
  # Full caching implementation would require more infrastructure
  renderPlot(expr, ...)
}

#' Create a custom render function
#'
#' @param func Render function
#' @param outputFunc Output function (for Shiny module support)
#' @param outputArgs Output arguments
#' @param cacheHint Cache hint
#' @param cacheWriteHook Cache write hook
#' @param cacheReadHook Cache read hook
#' @return Custom render function
#' @export
createRenderFunction <- function(func, outputFunc = NULL, outputArgs = list(),
                                  cacheHint = list(), cacheWriteHook = NULL, cacheReadHook = NULL) {
  function(expr, env = parent.frame(), quoted = FALSE, ...) {
    if (!quoted) {
      expr <- substitute(expr)
    }
    
    # Create wrapper that calls the custom func
    wrapper <- function() {
      result <- eval(expr, envir = env)
      if (is.function(func)) {
        func(result)
      } else {
        result
      }
    }
    
    wrapper
  }
}

#' Size growth ratio policy for cached plots
#'
#' @param width Base width
#' @param height Base height  
#' @param growthRate Growth rate multiplier
#' @return Size policy function
#' @export
sizeGrowthRatio <- function(width = 400, height = 400, growthRate = 1.2) {
  function(dims) {
    list(
      width = max(width, dims$width * growthRate),
      height = max(height, dims$height * growthRate)
    )
  }
}

# Context for tracking current output assignment
# Use a global environment (not in namespace) to ensure it's shared
.output_assignment_context <- new.env(parent = emptyenv())

.get_output_assignment_context <- function() {
  .output_assignment_context
}

#' Set current output name (called during output$x <- assignment)
#'
#' @param name Output name
set_current_output_name <- function(name) {
  ctx <- .get_output_assignment_context()
  assign("current_output", name, envir = ctx)
}

#' Get current output name
#'
#' @return Output name or NULL
get_current_output_name <- function() {
  ctx <- .get_output_assignment_context()
  if (exists("current_output", envir = ctx, inherits = FALSE)) {
    get("current_output", envir = ctx, inherits = FALSE)
  } else {
    NULL
  }
}

#' Clear current output name
clear_current_output_name <- function() {
  ctx <- .get_output_assignment_context()
  if (exists("current_output", envir = ctx)) {
    rm("current_output", envir = ctx)
  }
}
