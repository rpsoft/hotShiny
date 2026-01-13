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
      node_id <- paste0("reactiveValues.", name)
      if (!exists(name, envir = self$node_ids)) {
        # Create input-like node for this value
        input_node <- self$builder$register_input(name, source = get_source_location())
        assign(name, input_node$id, envir = self$node_ids)
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
                  # Convert to character and trim whitespace
                  value_str <- trimws(as.character(value))
                  # Try to convert to numeric if the value looks numeric
                  # This allows numericInput values to work in arithmetic operations
                  if (value_str != "" && !is.na(suppressWarnings(as.numeric(value_str)))) {
                    # Value is numeric - return as numeric
                    return(as.numeric(value_str))
                  }
                  # Value is not numeric or is empty - return as string
                  return(value_str)
                }
                # If value is NULL in state manager, it means it hasn't been set yet.
                # Just return empty string. Do NOT call executor$get_value as that causes recursion.
              }
            }
          }

          # Default: return empty string
          return("")
        },
        error = function(e) {
          # Log error
          log_debug("[InputProxy] ERROR: ", conditionMessage(e), "\n", file = stderr())
          # If there's any error, return empty string
          return("")
        }
      )

      # Ensure we always return a value (string or numeric)
      if (is.null(result) || (is.character(result) && length(result) == 0)) {
        return("")
      }

      # Return result as-is (could be string or numeric)
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
  # We can set the output name on the proxy here
  if (inherits(value, "RenderProxy")) {
    value$set_output_name(name)
  }
  # Store in the environment
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
