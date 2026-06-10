# Reactive Values Implementation
# Reimplements reactiveValues() to build graph

#' Create reactive values
#'
#' @param ... Named values
#' @return ReactiveValues object
reactiveValues <- function(...) {
  builder <- get_graph_builder()

  if (is.null(builder)) {
    stop("reactiveValues() must be called within a hotShiny app context")
  }

  values <- list(...)
  rv <- ReactiveValues$new(builder = builder)

  # Initialize with provided values
  for (name in names(values)) {
    rv[[name]] <- values[[name]]
  }

  rv
}

#' Reactive Values
#'
#' Container for reactive values (like Shiny's reactiveValues)
ReactiveValues <- R6::R6Class("ReactiveValues",
  public = list(
    values = NULL,
    builder = NULL,
    node_ids = NULL,
    initialize = function(builder) {
      self$values <- new.env(parent = emptyenv())
      self$builder <- builder
      self$node_ids <- new.env(parent = emptyenv())
    },

    # Get a value
    get = function(name) {
      if (exists(name, envir = self$values)) {
        get(name, envir = self$values)
      } else {
        NULL
      }
    },

    # Set a value
    set = function(name, value) {
      assign(name, value, envir = self$values)

      # Register as input node if not exists
      if (!exists(name, envir = self$node_ids)) {
        # Create input-like node for this value
        input_node <- self$builder$register_input(name, source = get_source_location())
        assign(name, input_node$id, envir = self$node_ids)
      }

      # If the app is already running, a write to a reactive value must
      # propagate to outputs. hotShiny tracks dependencies statically and
      # cannot always attribute reactiveValues reads to a precise edge, so we
      # invalidate conservatively (re-run computed nodes). During initial graph
      # construction the app is not yet running and we skip this.
      executor <- tryCatch(get_executor(), error = function(e) NULL)
      app_running <- !is.null(executor) && !is.null(executor$app) && isTRUE(executor$app$running)
      if (app_running && exists(".hotshiny_invalidate_all", envir = asNamespace("hotShiny"))) {
        tryCatch(
          get(".hotshiny_invalidate_all", envir = asNamespace("hotShiny"))(),
          error = function(e) NULL
        )
      }
    },

    # Get all names
    names = function() {
      ls(envir = self$values)
    },

    # Get all values as list
    to_list = function() {
      as.list(self$values)
    }
  ),
  active = list(
    # Allow $ access
    `$` = function(x) {
      if (missing(x)) {
        return(self)
      }
      self$get(x)
    }
  )
)

# Make ReactiveValues work with $ and [[
`$.ReactiveValues` <- function(x, name) {
  if (exists(name, envir = x)) {
    return(NextMethod())
  }
  get("get", envir = x)(name)
}

`$<-.ReactiveValues` <- function(x, name, value) {
  if (exists(name, envir = x)) {
    assign(name, value, envir = x)
    return(x)
  }
  get("set", envir = x)(name, value)
  x
}

`[[.ReactiveValues` <- function(x, i, ...) {
  x$get(i)
}

`[[<-.ReactiveValues` <- function(x, i, ..., value) {
  x$set(i, value)
  x
}

# Coerce a stored input value to its natural R type.
#
# Values reported by the browser arrive as strings. To match Shiny we convert
# them back to their natural types: "TRUE"/"FALSE" -> logical, numeric-looking
# strings -> numeric, and leave everything else as character. Values that are
# already typed (set programmatically, or multi-element vectors such as a
# checkboxGroupInput selection) are returned unchanged.
coerce_input_value <- function(value) {
  if (is.null(value)) return(NULL)

  # Already a non-character type, or a multi-element vector: pass through.
  if (!is.character(value)) return(value)
  if (length(value) != 1) return(value)

  value_str <- trimws(value)
  if (value_str == "") return("")

  # Logical
  if (value_str %in% c("TRUE", "FALSE", "true", "false")) {
    return(as.logical(toupper(value_str)))
  }

  # Numeric (but not strings like "1e3foo"); guard against NA from coercion.
  num <- suppressWarnings(as.numeric(value_str))
  if (!is.na(num) && grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", value_str)) {
    return(num)
  }

  value_str
}

# Input object (for input$x access)
InputProxy <- R6::R6Class("InputProxy",
  public = list(
    builder = NULL,
    executor = NULL,
    initialize = function(builder, executor = NULL) {
      self$builder <- builder
      self$executor <- executor
    },

    # Get input value by name
    get = function(name) {
      # Return appropriate type (string or numeric), never throw an error
      # Numeric values are returned as numeric to support arithmetic operations
      # This prevents "object 'input.name' not found" errors

      # Validate name is a string
      if (!is.character(name) || length(name) != 1) {
        return("")
      }

      # Use tryCatch to ensure we never throw an error
      result <- tryCatch(
        {
          # Register input node first
          if (!is.null(self$builder)) {
            node <- self$builder$register_input(name)
            node_id <- node$id
            log_debug("[InputProxy] get: name=", name, ", node_id=", node_id, "\n", file = stderr())

            # Get value from executor
            executor <- self$executor
            if (is.null(executor)) {
              executor <- get_executor()
            }

            if (!is.null(executor)) {
              # Get value from state manager directly to avoid recursion
              # Access public field directly since there is no getter method
              state_manager <- executor$state_manager
              if (!is.null(state_manager)) {
                value <- state_manager$get_value(node_id)
                # Check if value exists
                if (!is.null(value)) {
                  return(coerce_input_value(value))
                }
                # If value is NULL in state manager the input has not been set
                # yet. Shiny semantics: input$x is NULL until the client reports
                # a value. Returning NULL (rather than "") lets req()/is.null()
                # work as written. Do NOT call executor$get_value (recursion).
              }
            }
          }

          # Default: input not available yet.
          return(NULL)
        },
        error = function(e) {
          # Log error
          log_debug("[InputProxy] ERROR: ", conditionMessage(e), "\n", file = stderr())
          # On any unexpected error, behave as if the input is unset.
          return(NULL)
        }
      )

      # Return result as-is (could be NULL, string, numeric, logical or vector)
      result
    }
  )
)

# Make InputProxy work with $
`$.InputProxy` <- function(x, name) {
  # Check if name is a member of the object (methods or fields)
  # R6 objects are environments, so we can check existence
  if (exists(name, envir = x)) {
    return(get(name, envir = x))
  }

  # Delegate to get() method for input values
  # Must retrieve 'get' without using $ to avoid recursion
  get_fn <- get("get", envir = x)
  get_fn(name)
}

# Output object (for output$x <- assignment)
# Use an environment to enable proper $<- method dispatch
OutputProxy <- function(builder) {
  output_env <- new.env(parent = emptyenv())
  class(output_env) <- "OutputProxy"
  attr(output_env, "builder") <- builder
  output_env
}

# Make OutputProxy work with $<-
# For environments, we need to handle this differently
# Note: R doesn't always call $<-.OutputProxy for environments, so we handle pending
# RenderProxies manually in runApp after server execution
`$<-.OutputProxy` <- function(x, name, value) {
  # When output$x <- renderText(...) is called, R evaluates renderText(...) first
  # So renderText returns a RenderProxy, then $<- is called with that proxy
  # We can set the output name on the proxy here.
  #
  # For module output proxies an `ns_prefix` attribute is present, in which case
  # the render node must be registered under the namespaced output id so the
  # client targets the right DOM element.
  prefix <- attr(x, "ns_prefix", exact = TRUE)
  target_name <- if (!is.null(prefix) && nzchar(prefix)) {
    paste(prefix, name, sep = "-")
  } else {
    name
  }
  if (inherits(value, "RenderProxy")) {
    value$set_output_name(target_name)
  }
  # Store in the environment (under the local name).
  assign(name, value, envir = x)
  x
}

# Make OutputProxy work with $
`$.OutputProxy` <- function(x, name) {
  if (exists(name, envir = x, inherits = FALSE)) {
    get(name, envir = x, inherits = FALSE)
  } else {
    NULL
  }
}
