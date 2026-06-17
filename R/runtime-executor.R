# Runtime Executor
# Executes the reactive graph

#' Reactive Executor
#'
#' Executes reactive graph nodes in correct order
ReactiveExecutor <- R6::R6Class("ReactiveExecutor",
  public = list(
    graph = NULL,
    state_manager = NULL,
    execution_env = NULL,
    helper_functions = NULL,
    app = NULL,
    builder = NULL,
    session = NULL,

    # Set app reference (called from HotShinyApp)
    set_app = function(app) {
      self$app <- app
    },
    initialize = function(graph, state_manager = NULL, builder = NULL) {
      self$graph <- graph
      self$state_manager <- if (is.null(state_manager)) StateManager$new() else state_manager
      # Use globalenv() as parent to ensure access to package functions
      # This allows functions like hotShiny::iframe or user helpers to be found
      self$execution_env <- new.env(parent = globalenv())
      self$helper_functions <- new.env(parent = emptyenv())
      self$builder <- builder
      # Load helper functions
      self$load_helper_functions()
    },

    # Load helper functions needed for execution
    load_helper_functions = function() {
      ns <- asNamespace("hotShiny")
      base_path <- getwd()
      if (!file.exists(file.path(base_path, "R"))) {
        base_path <- NULL
      }

      load_env <- new.env(parent = ns)
      if (!is.null(base_path)) {
        # Load dependency tracker which has ast_to_expr
        file_path <- file.path(base_path, "R/ir-dependency-tracker.R")
        if (file.exists(file_path)) {
          sys.source(file_path, envir = load_env)
        }
      }

      # Store helper functions
      if (exists("ast_to_expr", envir = load_env)) {
        assign("ast_to_expr", get("ast_to_expr", envir = load_env), envir = self$helper_functions)
      } else if (exists("ast_to_expr", envir = ns)) {
        assign("ast_to_expr", get("ast_to_expr", envir = ns), envir = self$helper_functions)
      }
    },

    # Get a helper function
    get_helper_function = function(name) {
      if (!is.null(self$helper_functions) && exists(name, envir = self$helper_functions)) {
        return(get(name, envir = self$helper_functions))
      }
      # Try namespace
      ns <- asNamespace("hotShiny")
      if (exists(name, envir = ns)) {
        return(get(name, envir = ns))
      }
      # Try loading it
      self$load_helper_functions()
      if (!is.null(self$helper_functions) && exists(name, envir = self$helper_functions)) {
        return(get(name, envir = self$helper_functions))
      }
      stop("Helper function '", name, "' not found")
    },

    # Execute the graph
    execute = function() {
      log_debug("[Executor] execute() called\n", file = stderr())
      # Get graph from builder if available (graph may be updated after executor creation)
      graph_to_use <- self$graph
      if (!is.null(self$builder)) {
        graph_from_builder <- self$builder$get_graph()
        if (!is.null(graph_from_builder)) {
          graph_to_use <- graph_from_builder
          # Update executor's graph reference to latest
          self$graph <- graph_to_use
          log_debug("[Executor] execute() using graph from builder, updated reference\n", file = stderr())
        }
      }

      # Check graph
      if (is.null(graph_to_use)) {
        log_debug("[Executor] execute() ERROR: graph is NULL!\n", file = stderr())
        return(invisible(NULL))
      }
      all_nodes_check <- graph_to_use$get_all_nodes()
      log_debug("[Executor] execute() graph has", length(all_nodes_check), "nodes\n", file = stderr())
      # Get topological sort
      execution_order <- graph_to_use$topological_sort()
      log_debug("[Executor] Execution order:", paste(execution_order, collapse = ", "), " (length:", length(execution_order), ")\n", file = stderr())

      # If execution order is empty, log why
      if (length(execution_order) == 0) {
        log_debug("[Executor] WARNING: Execution order is empty! Graph nodes:", length(all_nodes_check), ", Edges:", length(graph_to_use$edges), "\n", file = stderr())
        if (length(all_nodes_check) > 0) {
          log_debug("[Executor] Node IDs:", paste(sapply(all_nodes_check, function(n) if (inherits(n, "ReactiveNode")) n$id else if (is.list(n)) n$id else "unknown"), collapse = ", "), "\n", file = stderr())
        }
        if (length(graph_to_use$edges) > 0) {
          log_debug("[Executor] Edge from/to:", paste(sapply(graph_to_use$edges, function(e) paste(e$from, "->", e$to)), collapse = ", "), "\n", file = stderr())
        }
      }

      # Execute nodes in order
      executed_count <- 0
      for (node_id in execution_order) {
        node <- graph_to_use$get_node(node_id)
        if (is.null(node)) {
          log_debug("[Executor] Node", node_id, "not found\n", file = stderr())
          next
        }

        # Check if node is dirty or needs execution
        should_exec <- self$should_execute(node)
        log_debug("[Executor] Node", node_id, "type=", node$type, ", should_execute=", should_exec, "\n", file = stderr())
        if (should_exec) {
          log_debug("[Executor] Executing node", node_id, "\n", file = stderr())
          self$execute_node(node)
          executed_count <- executed_count + 1
        }
      }

      log_debug("[Executor] execute() executed", executed_count, "nodes\n", file = stderr())

      # Clear dirty flags AFTER all execution is complete
      self$state_manager$clear_all_dirty()
      log_debug("[Executor] execute() completed, cleared dirty flags\n", file = stderr())
    },

    # Check if node should be executed
    should_execute = function(node) {
      # Always execute if dirty
      if (self$state_manager$is_dirty(node$id)) {
        log_debug("[Executor] should_execute: node", node$id, "is dirty\n", file = stderr())
        return(TRUE)
      }

      # Check if any dependency is dirty
      for (dep_id in node$deps) {
        if (self$state_manager$is_dirty(dep_id)) {
          log_debug("[Executor] should_execute: node", node$id, "has dirty dependency", dep_id, "\n", file = stderr())
          return(TRUE)
        }
      }

      # CRITICAL: For render nodes, execute if they have no value yet (initial execution)
      # This ensures plots and other outputs are rendered on first load
      if (inherits(node, "RenderNode")) {
        current_value <- self$state_manager$get_value(node$id)
        # Treat NULL / empty / NA / blank-string as "no value yet". Guard against
        # NA and length != 1 so the `if` never sees an NA or length>1 logical
        # (which throws "missing value where TRUE/FALSE needed").
        no_value <- is.null(current_value) || length(current_value) == 0 ||
          (length(current_value) == 1 &&
             (is.na(current_value) ||
                (is.character(current_value) && trimws(current_value) == "")))
        if (isTRUE(no_value)) {
          log_debug("[Executor] should_execute: node", node$id, "is RenderNode with no value, executing for initial render\n", file = stderr())
          return(TRUE)
        }
      }

      log_debug("[Executor] should_execute: node", node$id, "does NOT need execution\n", file = stderr())
      return(FALSE)

      # For observers, check if they need to run
      if (inherits(node, "ObserverNode")) {
        destroyed <- if (is.null(node$metadata$destroyed)) FALSE else node$metadata$destroyed
        if (!node$suspended && !destroyed) {
          # Check if dependencies changed
          return(any(vapply(node$deps, function(d) {
            self$state_manager$is_dirty(d)
          }, logical(1))))
        }
      }

      FALSE
    },

    # Execute a single node
    execute_node = function(node) {
      log_debug("[Executor] execute_node: executing", node$id, "type=", node$type, "\n", file = stderr())
      tryCatch(
        {
          self$state_manager$set_execution_state(node$id, "executing")

          value <- switch(node$type,
            "input" = {
              # Input nodes are set externally
              val <- self$state_manager$get_value(node$id)
              log_debug("[Executor] execute_node: input node", node$id, "value='", val, "'\n", file = stderr())
              val
            },
            "reactive" = {
              val <- self$execute_reactive(node)
              log_debug("[Executor] execute_node: reactive node", node$id, "returned value='", if (is.null(val)) "NULL" else val, "'\n", file = stderr())
              val
            },
            "observe" = {
              self$execute_observer(node)
              NULL # Observers don't return values
            },
            "render" = {
              val <- self$execute_render(node)
              log_debug("[Executor] execute_node: render node", node$id, "returned value='", if (is.null(val)) "NULL" else val, "'\n", file = stderr())
              val
            },
            "output" = {
              # Output nodes get values from render nodes
              val <- self$state_manager$get_value(node$id)
              log_debug("[Executor] execute_node: output node", node$id, "value='", if (is.null(val)) "NULL" else val, "'\n", file = stderr())
              val
            },
            {
              log_debug("[Executor] execute_node: WARNING - unknown node type:", node$type, "\n", file = stderr())
              warning("Unknown node type: ", node$type)
              NULL
            }
          )

          # Store value if not NULL
          if (!is.null(value)) {
            self$state_manager$set_value(node$id, value)
            log_debug("[Executor] execute_node: stored value '", value, "' for", node$id, "\n", file = stderr())
          } else {
            log_debug("[Executor] execute_node: value is NULL for", node$id, ", not storing\n", file = stderr())
          }

          # Don't clear dirty flag here - let execute() clear all at once
          # This ensures dependencies stay dirty until all nodes are executed
          # self$state_manager$clear_dirty(node$id)
          self$state_manager$set_execution_state(node$id, "completed")
        },
        error = function(e) {
          error_msg <- conditionMessage(e)

          # Catch all errors and handle them internally
          # DO NOT send errors to WebSocket clients
          # This prevents error spam and internal errors from being exposed

          # For input-related errors, store empty string
          if (grepl("object.*input", error_msg, ignore.case = TRUE)) {
            # Input-related error - store empty string and mark as completed
            self$state_manager$set_value(node$id, "")
            self$state_manager$set_execution_state(node$id, "completed")
            return(invisible(NULL))
          }

          # For other errors, log but don't send to client
          self$state_manager$set_execution_state(node$id, list(
            status = "error",
            error = error_msg
          ))
          warning("Error executing node ", node$id, ": ", error_msg)

          # Store empty string to prevent NULL values
          self$state_manager$set_value(node$id, "")
        }
      )
    },

    # Helper to create input proxy
    create_input_proxy = function() {
      app <- self$get_app()
      if (is.null(app)) {
        return(NULL)
      }

      # Get InputProxy class from namespace
      ns <- asNamespace("hotShiny")
      InputProxyClass <- if (exists("InputProxy", envir = ns)) {
        get("InputProxy", envir = ns)
      } else {
        # Fallback: try to load it
        base_path <- getwd()
        if (!file.exists(file.path(base_path, "R"))) {
          base_path <- system.file(package = "hotShiny")
        }
        load_env <- new.env(parent = ns)
        values_file <- file.path(base_path, "R/core-values.R")
        if (file.exists(values_file)) {
          sys.source(values_file, envir = load_env)
          if (exists("InputProxy", envir = load_env)) {
            get("InputProxy", envir = load_env)
          } else {
            NULL
          }
        } else {
          NULL
        }
      }

      if (is.null(InputProxyClass)) {
        return(NULL)
      }

      builder_to_use <- self$builder
      if (is.null(builder_to_use) && !is.null(app)) {
        builder_to_use <- app$builder
      }
      if (!is.null(builder_to_use)) {
        return(InputProxyClass$new(builder = builder_to_use, executor = self))
      }

      NULL
    },

    # Execute a reactive expression
    execute_reactive = function(node) {
      if (is.null(node$expr)) {
        log_debug("[Executor] execute_reactive: node", node$id, "has no expr\n", file = stderr())
        return(NULL)
      }

      log_debug("[Executor] execute_reactive: executing node", node$id, "\n", file = stderr())

      # Reconstruct expression from AST
      # Get ast_to_expr function
      ast_to_expr_fn <- self$get_helper_function("ast_to_expr")
      expr <- ast_to_expr_fn(node$expr)
      log_debug("[Executor] execute_reactive: reconstructed expr:", paste(deparse(expr), collapse = " "), "\n", file = stderr())

      # Get dependency values
      # Separate input dependencies from reactive dependencies
      input_deps <- character(0)
      reactive_deps <- list()

      for (dep_id in node$deps) {
        if (grepl("^input\\.", dep_id)) {
          # Input dependency - don't assign as variable, use input proxy
          input_deps <- c(input_deps, dep_id)
        } else {
          # Reactive dependency - need to map node ID to variable name
          # First try to get the variable name from reactive_sources registry
          dep_name <- NULL
          app <- self$get_app()
          if (!is.null(app) && !is.null(app$builder) && !is.null(app$builder$reactive_context)) {
            reactive_sources <- tryCatch(
              {
                app$builder$reactive_context$reactive_sources
              },
              error = function(e) NULL
            )
            if (!is.null(reactive_sources) && is.environment(reactive_sources)) {
              # Find the variable name that maps to this node ID
              for (var_name in ls(envir = reactive_sources, all.names = TRUE)) {
                if (exists(var_name, envir = reactive_sources, inherits = FALSE)) {
                  mapped_id <- get(var_name, envir = reactive_sources, inherits = FALSE)
                  if (mapped_id == dep_id) {
                    dep_name <- var_name
                    break
                  }
                }
              }
            }
          }
          # Fallback: use the node ID without prefix as name
          if (is.null(dep_name)) {
            dep_name <- gsub("^[^.]+\\.", "", dep_id)
          }
          dep_value <- self$get_value(dep_id)
          reactive_deps[[dep_name]] <- dep_value
        }
      }

      # Create evaluation environment
      # Use globalenv() as parent to ensure package functions are visible
      # Parent the evaluation environment to the node's captured closure (the
      # server body). This makes live objects reachable by reconstructed
      # expressions: reactiveVal() functions, reactiveValues objects, and any
      # user-defined helpers/variables defined in the server. The input proxy
      # and reactive getter functions below are overlaid as direct bindings so
      # they shadow the originals (named reactives are not callable on their
      # own; isolate'd inputs still read from the shared state). Falls back to
      # globalenv() when no closure was captured.
      eval_env <- new.env(parent = if (!is.null(node$env) && is.environment(node$env)) node$env else globalenv())

      # Add reactive dependency values to environment
      # Make them callable (like Shiny's reactive expressions)
      # In Shiny, reactive() returns a function that can be called with ()
      for (name in names(reactive_deps)) {
        # Get the actual reactive node ID to retrieve the value dynamically
        dep_id <- NULL
        app <- self$get_app()
        if (!is.null(app) && !is.null(app$builder) && !is.null(app$builder$reactive_context)) {
          reactive_sources <- tryCatch(
            {
              app$builder$reactive_context$reactive_sources
            },
            error = function(e) NULL
          )
          if (!is.null(reactive_sources) && is.environment(reactive_sources)) {
            if (exists(name, envir = reactive_sources, inherits = FALSE)) {
              dep_id <- get(name, envir = reactive_sources, inherits = FALSE)
            }
          }
        }

        # Create a closure that gets the current value of the reactive
        # This ensures we always get the latest value when the function is called
        local({
          n <- name
          dep_node_id <- dep_id
          exec <- self
          # Create a function that retrieves the current value
          reactive_fn <- function() {
            if (!is.null(dep_node_id)) {
              exec$get_value(dep_node_id)
            } else {
              ""
            }
          }
          assign(n, reactive_fn, envir = eval_env)
        })
      }

      # Add input proxy so input$x can be accessed in reactive expressions
      # This MUST be done before evaluating the expression
      input_proxy <- self$create_input_proxy()
      if (!is.null(input_proxy)) {
        assign("input", input_proxy, envir = eval_env)
      } else {
        log_debug("[Executor] execute_reactive: WARNING - input proxy is NULL, using dummy\n", file = stderr())
        # If we can't create input proxy, create a dummy one that returns empty strings
        # This prevents "object 'input' not found" errors
        dummy_input <- structure(
          list(get = function(name) ""),
          class = "InputProxy"
        )
        assign("input", dummy_input, envir = eval_env)
      }

      # Expose the session object so reactive expressions can reach
      # session$ns(), session$clientData, etc.
      if (!is.null(self$session)) {
        assign("session", self$session, envir = eval_env)
      }

      log_debug("[Executor] execute_reactive: evaluating expr:", paste(deparse(expr), collapse = " "), "\n", file = stderr())
      # Evaluate expression with error handling
      result <- tryCatch(
        {
          eval(expr, envir = eval_env)
        },
        error = function(e) {
          # Catch all errors and return empty string
          # This prevents errors from propagating to the client
          error_msg <- conditionMessage(e)
          log_debug("[Executor] execute_reactive: ERROR evaluating:", error_msg, "\n", file = stderr())

          # Log warning for debugging, but don't send to client
          if (grepl("object.*input", error_msg, ignore.case = TRUE)) {
            # Input-related error - return empty string silently
            return("")
          } else {
            # Other errors - log but still return empty string
            warning("Error evaluating reactive expression: ", error_msg)
            return("")
          }
        }
      )

      # Ensure result is not NULL
      if (is.null(result)) {
        result <- ""
      }

      log_debug("[Executor] execute_reactive: result='", result, "'\n", file = stderr())

      # Store result in state manager
      self$state_manager$set_value(node$id, result)

      result
    },

    # Execute an observer
    execute_observer = function(node) {
      if (is.null(node$expr)) {
        return(NULL)
      }

      # Check if suspended
      if (node$suspended) {
        return(NULL)
      }

      # Check if once and already executed
      executed <- if (is.null(node$metadata$executed)) FALSE else node$metadata$executed
      if (node$once && executed) {
        return(NULL)
      }

      # Reconstruct expression
      ast_to_expr_fn <- self$get_helper_function("ast_to_expr")
      expr <- ast_to_expr_fn(node$expr)

      # Build an evaluation environment that mirrors what reactive/render get:
      # the input proxy, getter functions for any reactive dependencies, and the
      # session object. Without this, observers (and observeEvent handlers) could
      # not read input$x or call session$sendCustomMessage().
      # Parent the evaluation environment to the node's captured closure (the
      # server body). This makes live objects reachable by reconstructed
      # expressions: reactiveVal() functions, reactiveValues objects, and any
      # user-defined helpers/variables defined in the server. The input proxy
      # and reactive getter functions below are overlaid as direct bindings so
      # they shadow the originals (named reactives are not callable on their
      # own; isolate'd inputs still read from the shared state). Falls back to
      # globalenv() when no closure was captured.
      eval_env <- new.env(parent = if (!is.null(node$env) && is.environment(node$env)) node$env else globalenv())

      for (dep_id in node$deps) {
        if (grepl("^input\\.", dep_id)) next
        dep_name <- NULL
        app <- self$get_app()
        if (!is.null(app) && !is.null(app$builder) && !is.null(app$builder$reactive_context)) {
          reactive_sources <- tryCatch(app$builder$reactive_context$reactive_sources,
                                        error = function(e) NULL)
          if (!is.null(reactive_sources) && is.environment(reactive_sources)) {
            for (var_name in ls(envir = reactive_sources, all.names = TRUE)) {
              if (identical(get(var_name, envir = reactive_sources, inherits = FALSE), dep_id)) {
                dep_name <- var_name
                break
              }
            }
          }
        }
        if (is.null(dep_name)) dep_name <- gsub("^[^.]+\\.", "", dep_id)
        local({
          dep_node_id <- dep_id
          exec <- self
          assign(dep_name, function() exec$get_value(dep_node_id), envir = eval_env)
        })
      }

      input_proxy <- self$create_input_proxy()
      if (!is.null(input_proxy)) {
        assign("input", input_proxy, envir = eval_env)
      }
      if (!is.null(self$session)) {
        assign("session", self$session, envir = eval_env)
      }

      # For observeEvent, honour ignoreNULL: skip the handler when the event
      # expression evaluates to NULL/empty (matches Shiny's default).
      if (isTRUE(node$metadata$event_driven) && !is.null(node$metadata$event_expr)) {
        ignore_null <- if (is.null(node$metadata$ignoreNULL)) TRUE else isTRUE(node$metadata$ignoreNULL)
        if (ignore_null) {
          event_val <- tryCatch(
            eval(ast_to_expr_fn(node$metadata$event_expr), envir = eval_env),
            error = function(e) NULL
          )
          truthy <- !is.null(event_val) && !(length(event_val) == 0) &&
            !(is.character(event_val) && length(event_val) == 1 && !nzchar(event_val))
          if (!truthy) {
            if (node$once) node$metadata$executed <- TRUE
            return(NULL)
          }
        }
      }

      # Evaluate (side effects), swallowing silent errors from req()/validate().
      tryCatch(
        eval(expr, envir = eval_env),
        shiny.silent.error = function(e) NULL,
        error = function(e) {
          log_debug("[Executor] execute_observer: error:", conditionMessage(e), "\n", file = stderr())
        }
      )

      # Mark as executed if once
      if (node$once) {
        node$metadata$executed <- TRUE
      }

      NULL
    },

    # Execute a render function
    execute_render = function(node) {
      # Debug: Check if node has render_type field
      render_type_val <- tryCatch(node$render_type, error = function(e) {
        log_debug("[Executor] execute_render: ERROR accessing node$render_type:", conditionMessage(e), "\n", file = stderr())
        NULL
      })
      log_debug("[Executor] execute_render: Starting render for node", node$id, "render_type=", if (is.null(render_type_val)) "NULL" else render_type_val, "\n", file = stderr())
      log_debug("[Executor] execute_render: Node class:", class(node)[1], "\n", file = stderr())
      if (inherits(node, "RenderNode")) {
        log_debug("[Executor] execute_render: Node IS a RenderNode\n", file = stderr())
      } else {
        log_debug("[Executor] execute_render: Node is NOT a RenderNode, class:", paste(class(node), collapse = ", "), "\n", file = stderr())
      }
      if (is.null(node$expr)) {
        log_debug("[Executor] execute_render: node", node$id, "has no expr\n", file = stderr())
        return(NULL)
      }

      # Reconstruct expression
      ast_to_expr_fn <- self$get_helper_function("ast_to_expr")
      expr <- ast_to_expr_fn(node$expr)
      log_debug("[Executor] execute_render: Reconstructed expression for node", node$id, "\n", file = stderr())

      # Get dependency values
      # Separate input dependencies from reactive dependencies
      input_deps <- character(0)
      reactive_deps <- list()

      log_debug("[Executor] execute_render: node deps:", paste(node$deps, collapse = ", "), "\n", file = stderr())

      for (dep_id in node$deps) {
        if (grepl("^input\\.", dep_id)) {
          # Input dependency - don't assign as variable, use input proxy
          input_deps <- c(input_deps, dep_id)
        } else {
          # Reactive dependency - need to map node ID to variable name
          # First try to get the variable name from reactive_sources registry
          dep_name <- NULL
          app <- self$get_app()
          if (!is.null(app) && !is.null(app$builder) && !is.null(app$builder$reactive_context)) {
            reactive_sources <- tryCatch(
              {
                app$builder$reactive_context$reactive_sources
              },
              error = function(e) NULL
            )
            if (!is.null(reactive_sources) && is.environment(reactive_sources)) {
              # Find the variable name that maps to this node ID
              for (var_name in ls(envir = reactive_sources, all.names = TRUE)) {
                if (exists(var_name, envir = reactive_sources, inherits = FALSE)) {
                  mapped_id <- get(var_name, envir = reactive_sources, inherits = FALSE)
                  if (mapped_id == dep_id) {
                    dep_name <- var_name
                    break
                  }
                }
              }
            }
          }
          # Fallback: use the node ID without prefix as name
          if (is.null(dep_name)) {
            dep_name <- gsub("^[^.]+\\.", "", dep_id)
          }
          dep_value <- self$get_value(dep_id)
          reactive_deps[[dep_name]] <- dep_value
        }
      }

      # Create evaluation environment
      # Use globalenv() as parent to ensure package functions are visible
      # Parent the evaluation environment to the node's captured closure (the
      # server body). This makes live objects reachable by reconstructed
      # expressions: reactiveVal() functions, reactiveValues objects, and any
      # user-defined helpers/variables defined in the server. The input proxy
      # and reactive getter functions below are overlaid as direct bindings so
      # they shadow the originals (named reactives are not callable on their
      # own; isolate'd inputs still read from the shared state). Falls back to
      # globalenv() when no closure was captured.
      eval_env <- new.env(parent = if (!is.null(node$env) && is.environment(node$env)) node$env else globalenv())

      # Add reactive dependency values to environment
      # Make them callable (like Shiny's reactive expressions)
      # In Shiny, reactive() returns a function that can be called with ()
      for (name in names(reactive_deps)) {
        # Get the actual reactive node ID to retrieve the value dynamically
        dep_id <- NULL
        app <- self$get_app()
        if (!is.null(app) && !is.null(app$builder) && !is.null(app$builder$reactive_context)) {
          reactive_sources <- tryCatch(
            {
              app$builder$reactive_context$reactive_sources
            },
            error = function(e) NULL
          )
          if (!is.null(reactive_sources) && is.environment(reactive_sources)) {
            if (exists(name, envir = reactive_sources, inherits = FALSE)) {
              dep_id <- get(name, envir = reactive_sources, inherits = FALSE)
            }
          }
        }

        # Create a closure that gets the current value of the reactive
        # This ensures we always get the latest value when the function is called
        local({
          n <- name
          dep_node_id <- dep_id
          exec <- self
          # Create a function that retrieves the current value
          reactive_fn <- function() {
            if (!is.null(dep_node_id)) {
              exec$get_value(dep_node_id)
            } else {
              ""
            }
          }
          assign(n, reactive_fn, envir = eval_env)
        })
        log_debug("[Executor] execute_render: added reactive function", name, "->", if (is.null(dep_id)) "NULL" else dep_id, "\n", file = stderr())
      }

      # CRITICAL: Always check expression for function calls and ensure they're available
      # This handles cases where reactive_sources registry might not be populated yet,
      # or where reactive_deps has entries with wrong names (like reactive_0 instead of greeting)
      expr_str <- paste(deparse(expr), collapse = " ")
      log_debug("[Executor] execute_render: expression string:", expr_str, "\n", file = stderr())

      # Extract function calls from expression (simple pattern matching)
      # Look for patterns like greeting(), reactive_0(), etc.
      function_calls <- regmatches(expr_str, gregexpr("\\b[a-zA-Z_][a-zA-Z0-9_]*\\(\\)", expr_str))[[1]]
      function_names <- gsub("\\(\\)", "", function_calls)
      log_debug("[Executor] execute_render: found function calls in expression:", paste(function_names, collapse = ", "), "\n", file = stderr())

      # For each function call, ensure it's available in eval_env
      app <- self$get_app()
      if (!is.null(app) && !is.null(app$builder) && !is.null(app$builder$reactive_context)) {
        reactive_sources <- tryCatch(
          {
            app$builder$reactive_context$reactive_sources
          },
          error = function(e) NULL
        )
        if (!is.null(reactive_sources) && is.environment(reactive_sources)) {
          reactive_var_names <- ls(envir = reactive_sources, all.names = TRUE)
          log_debug("[Executor] execute_render: reactive_sources registry has:", paste(reactive_var_names, collapse = ", "), "\n", file = stderr())

          for (func_name in function_names) {
            # Skip if already in eval_env
            if (exists(func_name, envir = eval_env, inherits = FALSE)) {
              log_debug("[Executor] execute_render: function", func_name, "already in eval_env\n", file = stderr())
              next
            }

            # Check if this function name is in reactive_sources
            if (exists(func_name, envir = reactive_sources, inherits = FALSE)) {
              func_node_id <- get(func_name, envir = reactive_sources, inherits = FALSE)
              log_debug("[Executor] execute_render: found", func_name, "in reactive_sources ->", func_node_id, "\n", file = stderr())
              # Add function to eval_env
              local({
                fname <- func_name
                dep_node_id <- func_node_id
                exec <- self
                reactive_fn <- function() {
                  if (!is.null(dep_node_id)) {
                    exec$get_value(dep_node_id)
                  } else {
                    ""
                  }
                }
                assign(fname, reactive_fn, envir = eval_env)
              })
              log_debug("[Executor] execute_render: added function", func_name, "to eval_env\n", file = stderr())
            } else {
              log_debug("[Executor] execute_render: WARNING - function", func_name, "not found in reactive_sources\n", file = stderr())
            }
          }
        }
      }

      # Add input proxy so input$x can be accessed in render expressions
      # This MUST be done before evaluating the expression
      input_proxy <- self$create_input_proxy()
      if (!is.null(input_proxy)) {
        assign("input", input_proxy, envir = eval_env)
        # Test if input proxy works
        test_input <- tryCatch(
          {
            input_proxy$get("name")
          },
          error = function(e) NULL
        )
        log_debug("[Executor] execute_render: input proxy test - input$name = '", if (is.null(test_input)) "NULL" else test_input, "'\n", file = stderr())
      } else {
        log_debug("[Executor] execute_render: WARNING - input proxy is NULL, using dummy\n", file = stderr())
        # If we can't create input proxy, create a dummy one that returns empty strings
        # This prevents "object 'input' not found" errors
        dummy_input <- structure(list(), class = "InputProxy")
        assign("input", dummy_input, envir = eval_env)
      }

      # Expose the session object to render expressions.
      if (!is.null(self$session)) {
        assign("session", self$session, envir = eval_env)
      }

      log_debug("[Executor] execute_render: evaluating expr:", paste(deparse(expr), collapse = " "), "\n", file = stderr())
      log_debug("[Executor] execute_render: available functions in eval_env:", paste(ls(envir = eval_env), collapse = ", "), "\n", file = stderr())
      
      # Get render_type from node or metadata
      render_type <- node$render_type
      if (is.null(render_type) && !is.null(node$metadata) && !is.null(node$metadata$render_type)) {
        render_type <- node$metadata$render_type
        log_debug("[Executor] execute_render: Got render_type from metadata: '", render_type, "'\n", file = stderr())
      }
      log_debug("[Executor] execute_render: node render_type = '", render_type, "' (class: ", class(render_type), ")\n", file = stderr())
      
      # For plot types, we need to capture the plot directly, so handle separately
      if (!is.null(render_type) && render_type == "plot") {
        log_debug("[Executor] execute_render: Processing plot render for node", node$id, "\n", file = stderr())
        # Capture plot as PNG and convert to base64
        temp_file <- tempfile(fileext = ".png")
        result <- tryCatch({
          # Get width and height from outputArgs if available
          width <- 400
          height <- 400
          if (!is.null(node$metadata) && !is.null(node$metadata$outputArgs)) {
            log_debug("[Executor] execute_render: Found outputArgs in metadata\n", file = stderr())
            if (!is.null(node$metadata$outputArgs$width) && node$metadata$outputArgs$width != "auto") {
              width <- as.numeric(gsub("px", "", as.character(node$metadata$outputArgs$width)))
            }
            if (!is.null(node$metadata$outputArgs$height) && node$metadata$outputArgs$height != "auto") {
              height <- as.numeric(gsub("px", "", as.character(node$metadata$outputArgs$height)))
            }
          }
          log_debug("[Executor] execute_render: Plot dimensions:", width, "x", height, "\n", file = stderr())
          
          # Close any existing graphics devices (except null device)
          while (dev.cur() > 1) {
            dev.off()
          }
          
          # Open PNG device
          log_debug("[Executor] execute_render: Opening PNG device:", temp_file, "\n", file = stderr())
          png(temp_file, width = width, height = height, units = "px", res = 72)
          # Evaluate the expression to generate the plot
          log_debug("[Executor] execute_render: Evaluating plot expression\n", file = stderr())
          eval(expr, envir = eval_env)
          dev.off()
          log_debug("[Executor] execute_render: PNG device closed\n", file = stderr())
          
          # Read the file and convert to base64
          if (file.exists(temp_file)) {
            file_size <- file.info(temp_file)$size
            log_debug("[Executor] execute_render: Plot file exists, size:", file_size, "bytes\n", file = stderr())
            if (file_size > 0) {
              plot_bytes <- readBin(temp_file, "raw", file_size)
              # Use base64enc if available
              if (requireNamespace("base64enc", quietly = TRUE)) {
                base64_plot <- base64enc::base64encode(plot_bytes)
                result_str <- paste0("data:image/png;base64,", base64_plot)
                log_debug("[Executor] execute_render: Plot encoded, base64 length:", nchar(base64_plot), "\n", file = stderr())
                result_str
              } else {
                error_msg <- "base64enc package not available, cannot encode plot. Install with: install.packages('base64enc')"
                log_debug("[Executor] execute_render:", error_msg, "\n", file = stderr())
                warning(error_msg)
                # Return error message so client can see what's wrong
                paste0("ERROR: ", error_msg)
              }
            } else {
              error_msg <- "plot file is empty"
              log_debug("[Executor] execute_render:", error_msg, "\n", file = stderr())
              paste0("ERROR: ", error_msg)
            }
          } else {
            error_msg <- "plot file does not exist after dev.off()"
            log_debug("[Executor] execute_render:", error_msg, "\n", file = stderr())
            paste0("ERROR: ", error_msg)
          }
        }, error = function(e) {
          error_msg <- conditionMessage(e)
          log_debug("[Executor] execute_render: ERROR capturing plot:", error_msg, "\n", file = stderr())
          log_debug("[Executor] execute_render: Error class:", class(e)[1], "\n", file = stderr())
          # Print traceback
          tryCatch({
            tb <- capture.output(traceback())
            log_debug("[Executor] execute_render: Error traceback:\n", paste(tb, collapse = "\n"), "\n", file = stderr())
          }, error = function(e2) {
            log_debug("[Executor] execute_render: Could not capture traceback\n", file = stderr())
          })
          # Make sure device is closed even on error
          if (dev.cur() > 1) {
            tryCatch({
              dev.off()
              log_debug("[Executor] execute_render: Closed graphics device after error\n", file = stderr())
            }, error = function(e2) {
              log_debug("[Executor] execute_render: Error closing device:", conditionMessage(e2), "\n", file = stderr())
            })
          }
          # Return error message as value so we can see what went wrong
          paste0("ERROR: ", error_msg)
        }, finally = {
          # Clean up temp file
          if (file.exists(temp_file)) {
            unlink(temp_file)
          }
        })
        formatted <- result
        log_debug("[Executor] execute_render: Plot result length:", if (is.character(formatted)) nchar(formatted) else "non-character", "\n", file = stderr())
        log_debug("[Executor] execute_render: Plot result preview:", if (is.character(formatted) && nchar(formatted) > 0) substr(formatted, 1, 100) else "EMPTY", "\n", file = stderr())
      } else {
        log_debug("[Executor] execute_render: Not a plot render (type='", render_type, "'), using normal evaluation path\n", file = stderr())
        # For non-plot types, evaluate normally
        result <- tryCatch(
          {
            eval_result <- eval(expr, envir = eval_env)
            # Check if result is NULL - this happens with plot() which returns NULL invisibly
            # If we get NULL and the expression contains plot(), we should treat it as a plot
            if (is.null(eval_result)) {
              expr_str <- paste(deparse(expr), collapse = " ")
              if (grepl("\\bplot\\s*\\(", expr_str)) {
                log_debug("[Executor] execute_render: WARNING - plot() expression returned NULL, but render_type is not 'plot'!\n", file = stderr())
                log_debug("[Executor] execute_render: Expression:", expr_str, "\n", file = stderr())
                log_debug("[Executor] execute_render: This suggests the node was not registered with render_type='plot'\n", file = stderr())
              }
            }
            eval_result
          },
          error = function(e) {
            # Catch all errors and return empty string
            # This prevents errors from propagating to the client
            error_msg <- conditionMessage(e)
            log_debug("[Executor] execute_render: ERROR evaluating:", error_msg, "\n", file = stderr())

            # Log warning for debugging, but don't send to client
            if (grepl("object.*input", error_msg, ignore.case = TRUE)) {
              # Input-related error - return empty string silently
              return("")
            } else {
              # Other errors - log but still return empty string
              warning("Error evaluating render expression: ", error_msg)
              return("")
            }
          }
        )

        # Check if result is NULL - this happens with plot() which returns NULL invisibly
        # If we get NULL and the expression contains plot(), treat it as a plot
        if (is.null(result)) {
          expr_str <- paste(deparse(expr), collapse = " ")
          if (grepl("\\bplot\\s*\\(", expr_str)) {
            log_debug("[Executor] execute_render: Detected plot() call returning NULL, treating as plot render\n", file = stderr())
            # Re-run as plot render
            temp_file <- tempfile(fileext = ".png")
            plot_result <- tryCatch({
              width <- 400
              height <- 400
              if (!is.null(node$metadata) && !is.null(node$metadata$outputArgs)) {
                if (!is.null(node$metadata$outputArgs$width) && node$metadata$outputArgs$width != "auto") {
                  width <- as.numeric(gsub("px", "", as.character(node$metadata$outputArgs$width)))
                }
                if (!is.null(node$metadata$outputArgs$height) && node$metadata$outputArgs$height != "auto") {
                  height <- as.numeric(gsub("px", "", as.character(node$metadata$outputArgs$height)))
                }
              }
              while (dev.cur() > 1) dev.off()
              png(temp_file, width = width, height = height, units = "px", res = 72)
              eval(expr, envir = eval_env)
              dev.off()
              if (file.exists(temp_file) && file.info(temp_file)$size > 0) {
                plot_bytes <- readBin(temp_file, "raw", file.info(temp_file)$size)
                if (requireNamespace("base64enc", quietly = TRUE)) {
                  base64_plot <- base64enc::base64encode(plot_bytes)
                  paste0("data:image/png;base64,", base64_plot)
                } else {
                  paste0("ERROR: base64enc package not available")
                }
              } else {
                paste0("ERROR: plot file not created")
              }
            }, error = function(e) {
              if (dev.cur() > 1) tryCatch(dev.off(), error = function(e) NULL)
              paste0("ERROR: ", conditionMessage(e))
            }, finally = {
              if (file.exists(temp_file)) unlink(temp_file)
            })
            result <- plot_result
          } else {
            result <- ""
          }
        }

        log_debug("[Executor] execute_render: result='", if (is.character(result) && nchar(result) > 100) paste0(substr(result, 1, 100), "...") else result, "' (length:", if (is.character(result)) nchar(result) else length(result), ")\n", file = stderr())

        # Format based on render type (use render_type variable we got earlier)
        formatted <- switch(render_type,
          "text" = as.character(result),
          "table" = result,
          "datatable" = result,
          "ui" = {
            if (inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list") || inherits(result, "html")) {
              as.character(result)
            } else {
              result
            }
          },
          result
        )
      }

      # Store formatted result
      self$state_manager$set_value(node$id, formatted)

      formatted
    },

    # Get value for a node (with caching)
    get_value = function(node_id) {
      # Validate node_id is a string
      if (!is.character(node_id) || length(node_id) != 1) {
        warning("Invalid node_id in get_value: ", deparse(node_id))
        return("")
      }

      tryCatch(
        {
          # Get graph from builder if available
          graph_to_use <- self$graph
          if (!is.null(self$builder)) {
            graph_from_builder <- self$builder$get_graph()
            if (!is.null(graph_from_builder)) {
              graph_to_use <- graph_from_builder
            }
          }
          # Get node to check type
          node <- graph_to_use$get_node(node_id)
          if (!is.null(node)) {
            # For InputNodes, don't execute - just return value from state manager
            if (inherits(node, "InputNode")) {
              value <- self$state_manager$get_value(node_id)
              if (is.null(value)) {
                return("")
              }
              return(value)
            }

            # For other nodes, check if they need execution (dirty or dependency dirty)
            # IMPORTANT: We need to execute the node if it's dirty OR if any dependency is dirty
            # This ensures reactive nodes are re-executed when their inputs change
            needs_execution <- self$should_execute(node)
            if (needs_execution) {
              # Execute the node - this will compute and store the new value
              self$execute_node(node)
            }

            # Get value from state manager (might be cached or just computed)
            value <- self$state_manager$get_value(node_id)
            if (is.null(value)) {
              # If value is still NULL after execution, return empty string
              return("")
            }
            return(value)
          }

          # Node doesn't exist - return empty string instead of NULL
          # This prevents "object not found" errors
          ""
        },
        error = function(e) {
          # If there's an error, return empty string
          # Don't print warning for missing input nodes (they're expected to be empty initially)
          if (!grepl("^input\\.", node_id)) {
            warning("Error getting value for node '", node_id, "': ", conditionMessage(e))
          }
          ""
        }
      )
    },

    # Set input value
    set_input = function(input_name, value) {
      tryCatch(
        {
          log_debug("[Executor] set_input: input_name=", input_name, ", value='", value, "'\n", file = stderr())
          node_id <- paste0("input.", input_name)

          # CRITICAL: Ensure input node exists in graph before setting value
          # Get graph from builder
          graph_to_use <- self$graph
          if (!is.null(self$builder)) {
            graph_from_builder <- self$builder$get_graph()
            if (!is.null(graph_from_builder)) {
              graph_to_use <- graph_from_builder
            }
          }

          # CRITICAL: Register input node if it doesn't exist
          # This must happen BEFORE we set the value and mark as dirty
          if (!is.null(self$builder)) {
            graph_to_check <- self$builder$get_graph()
            existing_node <- graph_to_check$get_node(node_id)
            if (is.null(existing_node)) {
              log_debug("[Executor] set_input: registering input node", node_id, "\n", file = stderr())
              input_node <- self$builder$register_input(input_name)
              log_debug("[Executor] set_input: input node registered, id=", input_node$id, "\n", file = stderr())
              # CRITICAL: Update executor's graph reference to include the new node
              # This ensures the graph has the input node when we do topological sort
              self$graph <- self$builder$get_graph()
              log_debug("[Executor] set_input: updated executor graph reference, now has", length(self$graph$get_all_nodes()), "nodes\n", file = stderr())
              # Verify node is now in graph
              verify_node <- self$graph$get_node(node_id)
              if (!is.null(verify_node)) {
                log_debug("[Executor] set_input: verified input node is in graph\n", file = stderr())
              } else {
                log_debug("[Executor] set_input: WARNING - input node not found in graph after registration!\n", file = stderr())
              }
            } else {
              log_debug("[Executor] set_input: input node", node_id, "already exists in graph\n", file = stderr())
              # Still update graph reference to ensure we have the latest
              self$graph <- self$builder$get_graph()
            }
          }

          # Normalise the incoming value before storing. The client may report a
          # scalar (most inputs) or an array (checkboxGroupInput,
          # selectInput(multiple=TRUE), sliderInput ranges); JSON arrays arrive as
          # lists. Preserve multi-element selections as character vectors so
          # input$x returns a vector as in Shiny; trim only scalar strings.
          if (is.list(value)) {
            value <- unlist(value, use.names = FALSE)
          }
          if (is.null(value) || length(value) == 0) {
            value_stored <- NULL
          } else if (length(value) == 1) {
            value_stored <- trimws(as.character(value))
          } else {
            value_stored <- as.character(value)
          }
          self$state_manager$set_value(node_id, value_stored)
          log_debug("[Executor] set_input: stored value for", node_id, "(length ", length(value_stored), ")\n", file = stderr())
          # Mark input node as dirty
          self$state_manager$mark_dirty(node_id)
          log_debug("[Executor] set_input: marked", node_id, "as dirty\n", file = stderr())

          # CRITICAL: Ensure graph reference is up-to-date before execution
          # The input node was just registered, so get fresh graph from builder
          if (!is.null(self$builder)) {
            self$graph <- self$builder$get_graph()
            log_debug("[Executor] set_input: refreshed graph reference before execute(), now has", length(self$graph$get_all_nodes()), "nodes\n", file = stderr())
          }

          # Trigger execution
          self$execute()
          log_debug("[Executor] set_input: execute() completed\n", file = stderr())
          # Send output updates via WebSocket if available
          self$send_output_updates()
          log_debug("[Executor] set_input: send_output_updates() completed\n", file = stderr())
        },
        error = function(e) {
          # Catch all errors during input processing
          # DO NOT send errors to client - they are internal errors
          error_msg <- conditionMessage(e)
          log_debug("[Executor] ERROR in set_input:", error_msg, "\n", file = stderr())

          # Log warning for debugging, but don't send to client
          warning("[Executor] Error setting input '", input_name, "': ", error_msg)

          # Always return silently to prevent error propagation
          return(invisible(NULL))
        }
      )
    },

    # Send output updates to clients
    send_output_updates = function() {
      # Get WebSocket server from app if available
      app <- self$get_app()
      if (is.null(app)) {
        message("[Executor] No app reference available for send_output_updates")
        return(invisible(NULL))
      }

      ws_server <- app$ws_server
      if (is.null(ws_server)) {
        message("[Executor] No WebSocket server available")
        return(invisible(NULL))
      }

      # Get WS_MESSAGE_TYPES
      ns <- asNamespace("hotShiny")
      base_path <- getwd()
      if (!file.exists(file.path(base_path, "R"))) {
        base_path <- system.file(package = "hotShiny")
      }
      load_env <- new.env(parent = ns)
      ws_file <- file.path(base_path, "R/server-websocket.R")
      if (file.exists(ws_file)) {
        sys.source(ws_file, envir = load_env)
      }

      WS_MESSAGE_TYPES <- if (exists("WS_MESSAGE_TYPES", envir = load_env)) {
        get("WS_MESSAGE_TYPES", envir = load_env)
      } else if (exists("WS_MESSAGE_TYPES", envir = ns)) {
        get("WS_MESSAGE_TYPES", envir = ns)
      } else {
        list(VALUE_UPDATE = "value_update")
      }

      # Get all output nodes and send their values
      # Get graph from builder if available (graph may be updated after executor creation)
      graph <- self$graph
      if (!is.null(self$builder)) {
        graph_from_builder <- self$builder$get_graph()
        if (!is.null(graph_from_builder)) {
          graph <- graph_from_builder
        }
      }
      all_nodes <- graph$get_all_nodes()
      log_debug("[Executor] send_output_updates: checking", length(all_nodes), "nodes\n", file = stderr())

      for (node in all_nodes) {
        # Check if it's a RenderNode - try multiple ways
        is_render <- FALSE
        output_name <- NULL

        # Check node type - RenderNode inherits from ReactiveNode which has type="render"
        node_type <- NULL
        if (inherits(node, "RenderNode")) {
          is_render <- TRUE
          node_type <- "render"
          output_name <- node$output_name
          # Also check metadata
          if (is.null(output_name) && !is.null(node$metadata) && !is.null(node$metadata$output_name)) {
            output_name <- node$metadata$output_name
          }
        } else if (!is.null(node$type) && node$type == "render") {
          is_render <- TRUE
          output_name <- node$output_name
          if (is.null(output_name) && !is.null(node$metadata) && !is.null(node$metadata$output_name)) {
            output_name <- node$metadata$output_name
          }
        }

        # Debug logging
        if (is_render) {
          log_debug("[Executor] send_output_updates: Found render node", node$id, "with output_name=", if (is.null(output_name)) "NULL" else output_name, "\n", file = stderr())
        }

        if (is_render) {
          node_id <- node$id

          # If output_name is still NULL, try to extract from node_id
          if (is.null(output_name) && grepl("^render\\.", node_id)) {
            output_name <- sub("^render\\.", "", node_id)
          }

          # Skip if we don't have an output_name
          if (is.null(output_name) || output_name == "") {
            next
          }

          # Get value - this will execute the node if needed
          value <- self$get_value(node_id)
          # For plot outputs, value might be a very long base64 string
          value_preview <- if (is.character(value) && nchar(value) > 100) {
            paste0(substr(value, 1, 100), "... [truncated, length=", nchar(value), "]")
          } else {
            value
          }
          log_debug("[Executor] send_output_updates: node", node_id, "value='", value_preview, "'\n", file = stderr())

          # Always send value updates, even if empty (so client can clear/update UI)
          # Convert NULL to empty string
          if (is.null(value)) {
            value <- ""
          }

          # Send value update with output name for client to find the element
          update_data <- list(
            node_id = node_id,
            output_name = output_name,
            value = value
          )
          value_preview2 <- if (is.character(value) && nchar(value) > 100) {
            paste0(substr(value, 1, 100), "... [truncated, length=", nchar(value), "]")
          } else {
            value
          }
          log_debug("[Executor] send_output_updates: sending update for", output_name, "with value='", value_preview2, "' to", length(ws_server$connections), "connections\n", file = stderr())
          # Send to all connections
          sent_count <- 0
          for (conn_id in names(ws_server$connections)) {
            conn <- ws_server$connections[[conn_id]]
            if (!is.null(conn)) {
              tryCatch(
                {
                  ws_server$send_message(conn, WS_MESSAGE_TYPES$VALUE_UPDATE, update_data)
                  sent_count <- sent_count + 1
                  log_debug("[Executor] send_output_updates: sent message to connection", conn_id, "\n", file = stderr())
                },
                error = function(e) {
                  log_debug("[Executor] send_output_updates: ERROR sending to", conn_id, ":", conditionMessage(e), "\n", file = stderr())
                }
              )
            }
          }
          log_debug("[Executor] send_output_updates: sent", sent_count, "messages total\n", file = stderr())
        }
      }
    },

    # Get app reference (stored when executor is created)
    get_app = function() {
      if (exists("app", envir = self)) {
        get("app", envir = self)
      } else {
        NULL
      }
    },

    # Get state manager
    get_state_manager = function() {
      self$state_manager
    }
  )
)
