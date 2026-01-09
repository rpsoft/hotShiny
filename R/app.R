# Main App Function
# Entry point for hotShiny applications

# Helper functions to get R6 classes
# Note: For proper functionality, the package should be installed with R CMD INSTALL
# This is a workaround for devtools::load_all() which may not load all files
.get_graph_builder_class <- function() {
  # Try namespace first
  ns <- asNamespace("hotShiny")
  if (exists("GraphBuilder", envir = ns, inherits = FALSE)) {
    return(get("GraphBuilder", envir = ns, inherits = FALSE))
  }

  # Need to load dependencies first
  # Create an environment with package namespace as parent
  load_env <- new.env(parent = ns)

  # Load required files in order (dependencies first)
  files_to_load <- c(
    "R/ir-node-types.R",
    "R/ir-dependency-tracker.R",
    "R/ir-graph-builder.R"
  )

  # Try current directory first (for development with devtools::load_all)
  base_path <- getwd()
  if (!file.exists(file.path(base_path, "R"))) {
    # Check if we're in the package source directory
    if (file.exists("DESCRIPTION") && file.exists("NAMESPACE")) {
      # We're in package root
    } else {
      # For installed package, R source files aren't available
      # Classes should be in namespace if package loaded correctly
      # If not, this is an error condition
      base_path <- NULL
    }
  }

  if (!is.null(base_path)) {
    for (file_rel in files_to_load) {
      file_path <- file.path(base_path, file_rel)
      if (file.exists(file_path)) {
        sys.source(file_path, envir = load_env)
      }
    }
  } else {
    # For installed packages, files should already be loaded
    # If classes aren't in namespace, there's a package loading issue
    stop("GraphBuilder class not found in namespace. This may indicate a package installation issue. Try reinstalling the package.")
  }

  if (exists("GraphBuilder", envir = load_env)) {
    return(get("GraphBuilder", envir = load_env))
  }

  stop("GraphBuilder class not found. Package files may not be loading correctly.")
}

.get_state_manager_class <- function() {
  ns <- asNamespace("hotShiny")
  if (exists("StateManager", envir = ns, inherits = FALSE)) {
    return(get("StateManager", envir = ns, inherits = FALSE))
  }

  load_env <- new.env(parent = ns)
  base_path <- getwd()
  if (!file.exists(file.path(base_path, "R"))) {
    base_path <- system.file(package = "hotShiny")
  }

  file_path <- file.path(base_path, "R/runtime-state-manager.R")
  if (!file.exists(file_path)) {
    file_path <- system.file("R/runtime-state-manager.R", package = "hotShiny", mustWork = FALSE)
  }
  if (file_path != "" && file.exists(file_path)) {
    sys.source(file_path, envir = load_env)
    if (exists("StateManager", envir = load_env)) {
      return(get("StateManager", envir = load_env))
    }
  }
  stop("StateManager class not found.")
}

.get_executor_class <- function() {
  ns <- asNamespace("hotShiny")
  if (exists("ReactiveExecutor", envir = ns, inherits = FALSE)) {
    return(get("ReactiveExecutor", envir = ns, inherits = FALSE))
  }

  load_env <- new.env(parent = ns)
  base_path <- getwd()
  if (!file.exists(file.path(base_path, "R"))) {
    base_path <- system.file(package = "hotShiny")
  }

  # Executor depends on StateManager, so load that first
  if (!exists("StateManager", envir = load_env)) {
    .get_state_manager_class() # This will load StateManager
  }

  file_path <- file.path(base_path, "R/runtime-executor.R")
  if (!file.exists(file_path)) {
    file_path <- system.file("R/runtime-executor.R", package = "hotShiny", mustWork = FALSE)
  }
  if (file_path != "" && file.exists(file_path)) {
    sys.source(file_path, envir = load_env)
    if (exists("ReactiveExecutor", envir = load_env)) {
      return(get("ReactiveExecutor", envir = load_env))
    }
  }
  stop("ReactiveExecutor class not found.")
}

.get_app_class <- function() {
  # HotShinyApp is in this file, should always be available
  HotShinyApp
}

#' Create a hotShiny application
#'
#' @param ui UI function or object
#' @param server Server function
#' @param ... Additional arguments
#' @return App object
app <- function(ui, server, ...) {
  # Create graph builder (get class and instantiate)
  GraphBuilderClass <- .get_graph_builder_class()
  builder <- GraphBuilderClass$new()
  # Access set_graph_builder from namespace
  ns <- asNamespace("hotShiny")
  if (exists("set_graph_builder", envir = ns)) {
    get("set_graph_builder", envir = ns)(builder)
  } else {
    # Fallback: source the file
    temp_env <- new.env(parent = ns)
    file_path <- file.path(getwd(), "R/core-reactive.R")
    if (file.exists(file_path)) {
      sys.source(file_path, envir = temp_env)
      if (exists("set_graph_builder", envir = temp_env)) {
        get("set_graph_builder", envir = temp_env)(builder)
      }
    }
  }

  # Create state manager
  StateManagerClass <- .get_state_manager_class()
  state_manager <- StateManagerClass$new()

  # Build graph from server function
  # This will happen as reactive(), observe(), etc. are called
  graph <- builder$get_graph()

  # Create executor
  ExecutorClass <- .get_executor_class()
  executor <- ExecutorClass$new(graph, state_manager, builder = builder)
  # Store app reference in executor for WebSocket communication
  # We'll set this after app is created
  # Access set_executor from namespace
  if (exists("set_executor", envir = ns)) {
    get("set_executor", envir = ns)(executor)
  } else {
    temp_env <- new.env(parent = ns)
    file_path <- file.path(getwd(), "R/runtime-state-manager.R")
    if (file.exists(file_path)) {
      sys.source(file_path, envir = temp_env)
      if (exists("set_executor", envir = temp_env)) {
        get("set_executor", envir = temp_env)(executor)
      }
    }
  }

  # Create app object
  AppClass <- .get_app_class()
  app_obj <- AppClass$new(
    ui = ui,
    server = server,
    builder = builder,
    executor = executor,
    state_manager = state_manager,
    ...
  )

  # Store app reference in executor for WebSocket communication
  executor$app <- app_obj

  app_obj
}

#' Run a hotShiny application from a file
#'
#' Loads an application file, creates the app, enables hot reload with file watching,
#' and starts the server. This is the recommended way to run hotShiny apps as it
#' automatically sets up file monitoring for hot reloading.
#'
#' @param app_file Path to the R file containing `ui` and `server` function definitions
#' @param port Port number to run the app on (default: 3838)
#' @param host Host address to bind to (default: "127.0.0.1")
#' @param watch_paths Optional additional paths to watch for changes. If NULL,
#'   watches the app file and its directory. Can be a character vector of file
#'   or directory paths.
#' @param ... Additional arguments passed to the app's `runApp()` method
#' @return The app object (invisibly)
#' @export
runApp <- function(app_file, port = 3838, host = "127.0.0.1", watch_paths = NULL, ...) {
  # Validate app_file
  if (missing(app_file) || is.null(app_file)) {
    stop("app_file is required. Please provide the path to your app file.")
  }
  
  if (!file.exists(app_file)) {
    stop("App file not found: ", app_file)
  }
  
  # Normalize the app file path
  app_file <- normalizePath(app_file)
  app_dir <- dirname(app_file)
  
  # Create an environment to load the app file
  app_env <- new.env(parent = globalenv())
  
  # Source the app file to get ui and server functions
  tryCatch({
    sys.source(app_file, envir = app_env)
  }, error = function(e) {
    stop("Error loading app file '", app_file, "': ", conditionMessage(e))
  })
  
  # Check if ui and server exist
  if (!exists("ui", envir = app_env)) {
    stop("App file '", app_file, "' must define a 'ui' function or object")
  }
  if (!exists("server", envir = app_env)) {
    stop("App file '", app_file, "' must define a 'server' function")
  }
  
  # Get ui and server
  ui <- get("ui", envir = app_env)
  server <- get("server", envir = app_env)
  
  # Create the app
  app_obj <- app(ui = ui, server = server)
  
  # Set up watch paths for hot reload
  # Default: watch the app file and its directory (for dependencies)
  if (is.null(watch_paths)) {
    watch_paths <- c(app_file, app_dir)
  } else {
    # Ensure app file and directory are included in watch paths
    # Normalize all paths for comparison
    normalized_watch <- vapply(watch_paths, normalizePath, character(1), mustWork = FALSE)
    normalized_app_file <- normalizePath(app_file)
    normalized_app_dir <- normalizePath(app_dir)
    
    # Add app file and directory if not already included
    if (!normalized_app_file %in% normalized_watch) {
      watch_paths <- c(watch_paths, app_file)
    }
    if (!normalized_app_dir %in% normalized_watch) {
      watch_paths <- c(watch_paths, app_dir)
    }
  }
  
  # Enable hot reload with file watching
  message("Enabling hot reload with file watching...")
  message("Watching: ", paste(watch_paths, collapse = ", "))
  enable_hot_reload(app_obj, watch_paths = watch_paths)
  
  # Run the app
  message("Starting hotShiny app from: ", app_file)
  app_obj$runApp(host = host, port = port, ...)
  
  invisible(app_obj)
}

#' HotShiny App
#'
#' Main application object
HotShinyApp <- R6::R6Class("HotShinyApp",
  public = list(
    ui = NULL,
    server = NULL,
    builder = NULL,
    executor = NULL,
    state_manager = NULL,
    server_func = NULL,
    running = NULL,
    server_handle = NULL,
    server_host = NULL,
    server_port = NULL,
    ws_server = NULL,
    initialize = function(ui, server, builder, executor, state_manager, ...) {
      self$ui <- ui
      self$server <- server
      self$builder <- builder
      self$executor <- executor
      self$state_manager <- state_manager
      self$running <- FALSE

      # Store server function for later execution
      self$server_func <- server
    },

    # Run the app
    runApp = function(host = "127.0.0.1", port = 3838, ...) {
      self$running <- TRUE

      # Ensure executor has helper functions loaded
      if (!is.null(self$executor)) {
        self$executor$load_helper_functions()
      }

      # Execute server function to build graph
      # This will call reactive(), observe(), etc.
      tryCatch({
        # Ensure helper functions are available in the execution environment
        # Source core files if needed to make functions accessible
        ns <- asNamespace("hotShiny")
        exec_env <- new.env(parent = parent.frame())
        load_env <- new.env(parent = ns)

        # List of functions to make available
        funcs_to_copy <- c(
          "get_graph_builder", "set_graph_builder", "reactive",
          "observe", "observeEvent", "renderText", "renderPlot",
          "renderTable", "renderDataTable", "renderUI", "reactiveValues",
          "extract_dependencies", "expr_to_ast", "get_source_location",
          "set_current_output_name", "get_current_output_name", "clear_current_output_name"
        )

        base_path <- getwd()
        if (!file.exists(file.path(base_path, "R"))) {
          base_path <- NULL
        }

        if (!is.null(base_path)) {
          # Load dependency tracker first (needed by reactive, observe, render)
          dep_tracker_file <- file.path(base_path, "R/ir-dependency-tracker.R")
          if (file.exists(dep_tracker_file)) {
            sys.source(dep_tracker_file, envir = load_env)
          }

          # Then load core files
          core_files <- c(
            "R/core-reactive.R",
            "R/core-observe.R",
            "R/core-render.R",
            "R/core-values.R"
          )
          for (file_rel in core_files) {
            file_path <- file.path(base_path, file_rel)
            if (file.exists(file_path)) {
              sys.source(file_path, envir = load_env)
            }
          }
          # Make functions available in execution environment
          for (func_name in funcs_to_copy) {
            if (exists(func_name, envir = load_env)) {
              assign(func_name, get(func_name, envir = load_env), envir = exec_env)
            }
          }
          # Also add extract_dependencies, ast_to_expr, and new_node_id if loaded
          for (func_name in c("extract_dependencies", "ast_to_expr", "new_node_id")) {
            if (exists(func_name, envir = load_env)) {
              assign(func_name, get(func_name, envir = load_env), envir = exec_env)
            }
          }
          # Also make new_node_id available to render functions' environments
          if (exists("new_node_id", envir = load_env)) {
            for (render_func_name in c("renderText", "renderPlot", "renderTable", "renderDataTable", "renderUI")) {
              if (exists(render_func_name, envir = exec_env)) {
                render_fn <- get(render_func_name, envir = exec_env)
                render_env <- environment(render_fn)
                if (!is.null(render_env)) {
                  assign("new_node_id", get("new_node_id", envir = load_env), envir = render_env)
                }
              }
            }
          }
        }

        # Set graph builder in execution environment
        if (exists("set_graph_builder", envir = exec_env)) {
          get("set_graph_builder", envir = exec_env)(self$builder)
        } else if (exists("set_graph_builder", envir = ns)) {
          get("set_graph_builder", envir = ns)(self$builder)
        }

        # Set executor in execution environment
        if (exists("set_executor", envir = exec_env)) {
          get("set_executor", envir = exec_env)(self$executor)
        } else if (exists("set_executor", envir = ns)) {
          get("set_executor", envir = ns)(self$executor)
        }

        # Create input and output proxies
        # Get classes from load_env or namespace
        InputProxyClass <- if (exists("InputProxy", envir = load_env)) {
          get("InputProxy", envir = load_env)
        } else {
          tryCatch(get("InputProxy", envir = ns), error = function(e) NULL)
        }
        OutputProxyClass <- if (exists("OutputProxy", envir = load_env)) {
          get("OutputProxy", envir = load_env)
        } else {
          tryCatch(get("OutputProxy", envir = ns), error = function(e) NULL)
        }

        if (is.null(InputProxyClass) || is.null(OutputProxyClass)) {
          # Load values.R to get proxy classes
          if (!is.null(base_path)) {
            values_file <- file.path(base_path, "R/core-values.R")
            if (file.exists(values_file)) {
              sys.source(values_file, envir = load_env)
              if (exists("InputProxy", envir = load_env)) {
                InputProxyClass <- get("InputProxy", envir = load_env)
              }
              if (exists("OutputProxy", envir = load_env)) {
                OutputProxyClass <- get("OutputProxy", envir = load_env)
              }
            }
          }
        }

        input <- InputProxyClass$new(builder = self$builder)
        # OutputProxy is now a function, not an R6 class
        if (is.function(OutputProxyClass)) {
          output <- OutputProxyClass(self$builder)
        } else {
          output <- OutputProxyClass$new(builder = self$builder)
        }
        session <- NULL # Would create session object

        # Call server function - attach functions to its environment
        if (is.function(self$server)) {
          # Attach functions to server function's environment
          server_env <- environment(self$server)
          if (is.null(server_env)) {
            server_env <- exec_env
          }
          # Copy functions to server environment
          for (func_name in funcs_to_copy) {
            if (exists(func_name, envir = exec_env)) {
              assign(func_name, get(func_name, envir = exec_env), envir = server_env)
            }
          }
          # Also copy extract_dependencies and ast_to_expr to server environment
          for (func_name in c("extract_dependencies", "ast_to_expr")) {
            if (exists(func_name, envir = exec_env)) {
              assign(func_name, get(func_name, envir = exec_env), envir = server_env)
            }
          }
          # Share output_assignment_context across all environments
          # Get the context from load_env if it exists
          if (exists("output_assignment_context", envir = load_env)) {
            shared_context <- get("output_assignment_context", envir = load_env)
            # Make sure all functions use the same context
            assign("output_assignment_context", shared_context, envir = server_env)
            # Also update render functions to use shared context
            for (render_func_name in c("renderText", "renderPlot", "renderTable", "renderDataTable", "renderUI")) {
              if (exists(render_func_name, envir = exec_env)) {
                render_fn <- get(render_func_name, envir = exec_env)
                render_env <- environment(render_fn)
                if (!is.null(render_env)) {
                  assign("output_assignment_context", shared_context, envir = render_env)
                }
              }
            }
          }
          # Make extract_dependencies available in reactive() function's environment
          if (exists("reactive", envir = exec_env)) {
            reactive_fn <- get("reactive", envir = exec_env)
            reactive_env <- environment(reactive_fn)
            if (!is.null(reactive_env) && exists("extract_dependencies", envir = exec_env)) {
              assign("extract_dependencies", get("extract_dependencies", envir = exec_env), envir = reactive_env)
            }
          }
          # Also set builder and executor in server environment
          if (exists("set_graph_builder", envir = exec_env)) {
            assign("set_graph_builder", get("set_graph_builder", envir = exec_env), envir = server_env)
          }
          if (exists("set_executor", envir = exec_env)) {
            assign("set_executor", get("set_executor", envir = exec_env), envir = server_env)
          }
          # Call server function normally
          self$server(input, output, session)

          # CRITICAL FIX 1: Register reactive expressions by variable name
          # The reactive variables are created in the server function's execution environment
          # We need to scan multiple environments to find them
          envs_to_scan <- list()
          if (!is.null(server_env)) {
            envs_to_scan <- c(envs_to_scan, list(server_env))
          }

          # Scan environments captured in graph nodes
          # This is more reliable than scanning parent frames which might be gone
          graph_nodes <- self$builder$get_graph()$get_all_nodes()
          for (node in graph_nodes) {
            if (!is.null(node$env) && is.environment(node$env)) {
              envs_to_scan <- c(envs_to_scan, list(node$env))
            }
          }

          # Scan all environments for ReactiveProxy objects
          cat("[App] scanning", length(envs_to_scan), "environments\n", file = stderr())
          for (scan_env in envs_to_scan) {
            tryCatch(
              {
                env_vars <- ls(envir = scan_env, all.names = TRUE)
                cat("[App] env has vars:", paste(env_vars, collapse = ", "), "\n", file = stderr())
                for (var_name in env_vars) {
                  tryCatch(
                    {
                      var_value <- get(var_name, envir = scan_env)
                      if (inherits(var_value, "ReactiveProxy")) {
                        cat("[App] found ReactiveProxy:", var_name, "id=", var_value$node_id, "\n", file = stderr())
                      }
                      if (inherits(var_value, "ReactiveProxy") && !is.null(var_value$node_id)) {
                        # Register this reactive by its variable name
                        if (!is.null(self$builder$reactive_context)) {
                          if (!exists("reactive_sources", envir = self$builder$reactive_context)) {
                            assign("reactive_sources", new.env(parent = emptyenv()), envir = self$builder$reactive_context)
                          }
                          reactive_sources <- get("reactive_sources", envir = self$builder$reactive_context)
                          # Only register if not already registered (avoid duplicates)
                          if (!exists(var_name, envir = reactive_sources, inherits = FALSE)) {
                            assign(var_name, var_value$node_id, envir = reactive_sources)
                            cat("[App] Registered reactive source:", var_name, "->", var_value$node_id, "\n", file = stderr())
                          }
                        }
                      }
                    },
                    error = function(e) {
                      # Ignore errors when accessing variables
                    }
                  )
                }
              },
              error = function(e) {
                # Ignore errors when scanning environments
                cat("[App] Error scanning env:", conditionMessage(e), "\n", file = stderr())
              }
            )
          }

          # CRITICAL FIX 2: Manually trigger set_output_name for any pending RenderProxies
          # This is needed because $<-.OutputProxy method dispatch doesn't work reliably for environments
          # R's $<- on environments doesn't always trigger our custom method
          if (is.environment(output)) {
            output_names <- ls(envir = output, all.names = TRUE)
            for (name in output_names) {
              val <- get(name, envir = output)
              if (inherits(val, "RenderProxy") && isTRUE(val$pending_output_name)) {
                # Manually trigger set_output_name to register the render node
                val$set_output_name(name)
              }
            }
          } else if (is.list(output)) {
            for (name in names(output)) {
              val <- output[[name]]
              if (inherits(val, "RenderProxy") && isTRUE(val$pending_output_name)) {
                val$set_output_name(name)
              }
            }
          }

          # CRITICAL FIX 1.5: Re-extract dependencies for render nodes now that reactives are registered
          # This MUST happen AFTER set_output_name so render nodes are registered
          # This fixes the issue where renderText dependencies were extracted before reactives were registered
          graph <- self$builder$get_graph()
          all_nodes <- graph$get_all_nodes()

          # Load extract_dependencies and ast_to_expr
          ns <- asNamespace("hotShiny")
          base_path <- getwd()
          if (!file.exists(file.path(base_path, "R"))) {
            base_path <- system.file(package = "hotShiny")
          }
          load_env <- new.env(parent = ns)
          dep_file <- file.path(base_path, "R/ir-dependency-tracker.R")
          if (file.exists(dep_file)) {
            sys.source(dep_file, envir = load_env)
          }
          extract_deps_fn <- if (exists("extract_dependencies", envir = load_env)) {
            get("extract_dependencies", envir = load_env)
          } else {
            get("extract_dependencies", envir = ns)
          }
          ast_to_expr_fn <- if (exists("ast_to_expr", envir = load_env)) {
            get("ast_to_expr", envir = load_env)
          } else {
            get("ast_to_expr", envir = ns)
          }

          cat("[App] Re-extraction: Found", length(all_nodes), "nodes total\n", file = stderr())
          reactive_count <- sum(sapply(all_nodes, function(n) inherits(n, "ReactiveExprNode")))
          cat("[App] Re-extraction: Found", reactive_count, "reactive nodes\n", file = stderr())
          
          # CRITICAL: Re-extract dependencies for BOTH ReactiveExprNodes AND RenderNodes
          # ReactiveExprNodes may depend on other reactive expressions (e.g., combined depends on sum_value)
          # These dependencies can only be resolved after all reactives are registered in reactive_sources
          for (node in all_nodes) {
            # Handle ReactiveExprNode (reactive expressions that may depend on other reactives)
            if (inherits(node, "ReactiveExprNode") && !is.null(node$expr)) {
              cat("[App] Re-extracting for ReactiveExprNode", node$id, ", current deps:", if (length(node$deps) == 0) "NONE" else paste(node$deps, collapse = ", "), "\n", file = stderr())
              expr <- ast_to_expr_fn(node$expr)
              cat("[App] Reconstructed expr:", paste(deparse(expr), collapse = " "), "\n", file = stderr())
              new_deps <- extract_deps_fn(expr)
              cat("[App] Extracted deps:", if (length(new_deps) == 0) "NONE" else paste(new_deps, collapse = ", "), "\n", file = stderr())
              # Update node dependencies
              node$deps <- new_deps
              # Rebuild edges for this node
              graph$edges <- Filter(function(e) e$to != node$id, graph$edges)
              for (dep_id in new_deps) {
                graph$edges <- c(graph$edges, list(list(from = dep_id, to = node$id)))
              }
              cat("[App] Updated ReactiveExprNode", node$id, "deps to:", paste(node$deps, collapse = ", "), "\n", file = stderr())
            }
            # Handle RenderNode (render expressions)
            if (inherits(node, "RenderNode") && !is.null(node$expr)) {
              cat("[App] Re-extracting for RenderNode", node$id, ", current deps:", if (length(node$deps) == 0) "NONE" else paste(node$deps, collapse = ", "), "\n", file = stderr())
              cat("[App] AST structure:", str(node$expr), "\n", file = stderr())
              # Re-extract dependencies now that reactive sources are registered
              expr <- ast_to_expr_fn(node$expr)
              cat("[App] Reconstructed expr:", paste(deparse(expr), collapse = " "), "\n", file = stderr())
              cat("[App] Expr is call?", rlang::is_call(expr), ", call_name:", if (rlang::is_call(expr)) rlang::call_name(expr) else "N/A", "\n", file = stderr())
              new_deps <- extract_deps_fn(expr)
              cat("[App] Extracted deps:", if (length(new_deps) == 0) "NONE" else paste(new_deps, collapse = ", "), "\n", file = stderr())
              # Update node dependencies
              node$deps <- new_deps
              # Rebuild edges for this node
              # Remove old edges for this node
              graph$edges <- Filter(function(e) e$to != node$id, graph$edges)
              # Add new edges
              for (dep_id in new_deps) {
                graph$edges <- c(graph$edges, list(list(from = dep_id, to = node$id)))
              }
              cat("[App] Updated RenderNode", node$id, "deps to:", paste(node$deps, collapse = ", "), "\n", file = stderr())
            }
          }

          # Check graph after server execution
          graph_after <- self$builder$get_graph()
          nodes_after <- graph_after$get_all_nodes()
          cat("[App] Graph after server execution:", length(nodes_after), "nodes\n", file = stderr())

          # CRITICAL: Update executor's graph reference to the latest graph
          # The graph was updated when nodes were added during server execution
          if (!is.null(self$executor)) {
            self$executor$graph <- graph_after
            cat("[App] Updated executor's graph reference\n", file = stderr())
          }
          if (length(nodes_after) > 0) {
            cat("[App] Node IDs:", paste(sapply(nodes_after, function(n) n$id), collapse = ", "), "\n", file = stderr())
            for (i in seq_along(nodes_after)) {
              n <- nodes_after[[i]]
              cat("[App] Node", i, ":", n$id, "- Class:", paste(class(n), collapse = ", "), "\n", file = stderr())
              if (inherits(n, "RenderNode")) {
                cat("[App]   Output name:", if (is.null(n$output_name)) "NULL" else n$output_name, "\n", file = stderr())
              }
              if (!is.null(n$deps) && length(n$deps) > 0) {
                cat("[App]   Dependencies:", paste(n$deps, collapse = ", "), "\n", file = stderr())
              }
            }
          }
        }
      }, finally = {
        # Clean up context
        # set_graph_builder(NULL)
        # set_executor(NULL)
      })

      # Start HTTP server using httpuv
      self$start_http_server(host, port, ...)

      self
    },

    # Start HTTP server
    start_http_server = function(host, port, ...) {
      # Store host and port for render_ui
      self$server_host <- host
      self$server_port <- port

      # Create WebSocket server
      ws_server <- NULL
      base_path <- getwd()
      if (!file.exists(file.path(base_path, "R"))) {
        base_path <- system.file(package = "hotShiny")
      }
      load_env <- new.env(parent = asNamespace("hotShiny"))
      ws_file <- file.path(base_path, "R/server-websocket.R")
      if (file.exists(ws_file)) {
        sys.source(ws_file, envir = load_env)
        if (exists("create_websocket_server", envir = load_env)) {
          ws_server <- get("create_websocket_server", envir = load_env)(self)
        }
      }

      # Store ws_server for later use
      self$ws_server <- ws_server
      # Also store in executor for sending updates
      if (!is.null(self$executor)) {
        # Set app reference in executor
        if (!is.null(self$executor) && is.function(self$executor$set_app)) {
          self$executor$set_app(self)
        } else {
          self$executor$app <- self
        }
      }

      # HTTP handlers - httpuv uses 'call' for HTTP and 'onWSOpen' for WebSocket
      handlers <- list(
        # HTTP request handler
        call = function(req) {
          tryCatch(
            {
              path <- req$PATH_INFO
              method <- req$REQUEST_METHOD

              # Serve main page
              if (method == "GET" && (path == "/" || path == "")) {
                # Render UI fresh each time (don't cache)
                ui_html <- tryCatch(
                  {
                    self$render_ui()
                  },
                  error = function(e) {
                    # If render fails, return error page
                    warning("Error in render_ui: ", conditionMessage(e))
                    paste0(
                      "<!DOCTYPE html><html><head><title>Error</title></head><body>",
                      "<h1>Error rendering UI</h1><p>", conditionMessage(e), "</p>",
                      "</body></html>"
                    )
                  }
                )
                return(list(
                  status = 200L,
                  headers = list("Content-Type" = "text/html; charset=UTF-8"),
                  body = ui_html
                ))
              }

              # Serve static files
              if (method == "GET" && grepl("^/static/", path)) {
                file_path <- sub("^/static/", "", path)
                base_path <- system.file("www", package = "hotShiny")
                if (base_path == "") {
                  base_path <- file.path(getwd(), "inst/www")
                }
                full_path <- file.path(base_path, file_path)

                if (file.exists(full_path)) {
                  ext <- tools::file_ext(file_path)
                  content_type <- switch(ext,
                    "js" = "application/javascript",
                    "css" = "text/css",
                    "html" = "text/html",
                    "json" = "application/json",
                    "png" = "image/png",
                    "jpg" = "image/jpeg",
                    "jpeg" = "image/jpeg",
                    "gif" = "image/gif",
                    "svg" = "image/svg+xml",
                    "text/plain"
                  )

                  return(list(
                    status = 200L,
                    headers = list("Content-Type" = content_type),
                    body = readBin(full_path, "raw", file.info(full_path)$size)
                  ))
                } else {
                  return(list(
                    status = 404L,
                    headers = list("Content-Type" = "text/plain"),
                    body = paste("File not found:", path)
                  ))
                }
              }

              # 404 for other paths
              list(
                status = 404L,
                headers = list("Content-Type" = "text/plain"),
                body = paste("Not found:", path)
              )
            },
            error = function(e) {
              # Return error response
              list(
                status = 500L,
                headers = list("Content-Type" = "text/plain"),
                body = paste("Server error:", conditionMessage(e))
              )
            }
          )
        },

        # WebSocket upgrade handler
        onWSOpen = function(ws) {
          if (!is.null(ws_server)) {
            ws_server$on_open(ws)
          }

          ws$onMessage(function(isBinary, message) {
            if (!is.null(ws_server)) {
              ws_server$on_message(ws, isBinary, message)
            }
          })

          ws$onClose(function() {
            if (!is.null(ws_server)) {
              ws_server$on_close(ws)
            }
          })
        }
      )

      # Start server
      message("Starting hotShiny app at http://", host, ":", port)
      message("Press ESC or Ctrl+C to stop the server")

      # Store server handle
      self$server_handle <- httpuv::startServer(
        host = host,
        port = port,
        app = handlers
      )

      # Run event loop
      httpuv::service(0)

      # Keep running until stopped
      iter_count <- 0
      while (self$running) {
        httpuv::service(100)
        # Process later callbacks (needed for hot reload file watcher)
        later::run_now()
        iter_count <- iter_count + 1
        if (iter_count %% 100 == 0) {
          cat("[EventLoop] Iteration", iter_count, "\n", file = stderr())
        }
        Sys.sleep(0.01)
      }

      # Stop server when done
      if (!is.null(self$server_handle)) {
        httpuv::stopServer(self$server_handle)
      }
    },

    # Render UI to HTML
    render_ui = function() {
      # Get UI functions from namespace or load them
      ns <- asNamespace("hotShiny")
      base_path <- getwd()
      
      # Create environment with all UI functions
      ui_env <- new.env(parent = globalenv())
      
      # Load UI functions from tags.R and inputs.R if available
      if (file.exists(file.path(base_path, "R/ui-tags.R"))) {
        sys.source(file.path(base_path, "R/ui-tags.R"), envir = ui_env)
      }
      if (file.exists(file.path(base_path, "R/ui-inputs.R"))) {
        sys.source(file.path(base_path, "R/ui-inputs.R"), envir = ui_env)
      }
      if (file.exists(file.path(base_path, "R/ui-outputs.R"))) {
        sys.source(file.path(base_path, "R/ui-outputs.R"), envir = ui_env)
      }
      if (file.exists(file.path(base_path, "R/ui-layout.R"))) {
        sys.source(file.path(base_path, "R/ui-layout.R"), envir = ui_env)
      }
      if (file.exists(file.path(base_path, "R/ui-navigation.R"))) {
        sys.source(file.path(base_path, "R/ui-navigation.R"), envir = ui_env)
      }
      
      # Also try namespace functions
      ui_funcs <- c("tag", "tags", "tagList", "div", "span", "p", "h1", "h2", "h3", 
                    "h4", "h5", "h6", "a", "br", "hr", "pre", "code", "img", "strong", 
                    "em", "ul", "ol", "li", "HTML", "textInput", "numericInput", 
                    "textOutput", "plotOutput", "textAreaInput", "passwordInput",
                    "selectInput", "checkboxInput", "checkboxGroupInput", "radioButtons",
                    "sliderInput", "dateInput", "dateRangeInput", "actionButton",
                    "actionLink", "fileInput", "fluidPage", "fluidRow", "fixedPage",
                    "sidebarLayout", "sidebarPanel", "mainPanel", "wellPanel",
                    "tabsetPanel", "tabPanel", "navbarPage", "column", "verbatimTextOutput",
                    "htmlOutput", "uiOutput", "imageOutput", "tableOutput", "dataTableOutput",
                    "downloadButton", "downloadLink", "icon", "helpText", "titlePanel",
                    "conditionalPanel", "fillPage", "fillRow", "fillCol")
      
      for (fn in ui_funcs) {
        if (!exists(fn, envir = ui_env, inherits = FALSE)) {
          if (exists(fn, envir = ns, inherits = FALSE)) {
            assign(fn, get(fn, envir = ns), envir = ui_env)
          }
        }
      }
      
      # Try to evaluate UI function
      ui_result <- tryCatch(
        {
          if (is.function(self$ui)) {
            ui_func <- self$ui
            environment(ui_func) <- ui_env
            result <- ui_func()
            result
          } else {
            self$ui
          }
        },
        error = function(e) {
          warning("Error rendering UI: ", conditionMessage(e))
          if (exists("div", envir = ui_env)) {
            get("div", envir = ui_env)(paste("Error rendering UI:", conditionMessage(e)))
          } else {
            list(name = "div", attribs = list(), children = list(paste("Error rendering UI:", conditionMessage(e))))
          }
        }
      )

      # Get host and port
      host <- if (!is.null(self$server_host)) self$server_host else "127.0.0.1"
      port <- if (!is.null(self$server_port)) self$server_port else 3838

      # Convert UI to HTML using tag_to_html from tags.R
      ui_html <- if (exists("tag_to_html", envir = ui_env)) {
        get("tag_to_html", envir = ui_env)(ui_result)
      } else {
        self$ui_to_html(ui_result)
      }

      # Build complete HTML page with Bootstrap 5
      html <- paste0(
        "<!DOCTYPE html>\n",
        "<html lang=\"en\">\n",
        "<head>\n",
        '  <meta charset="UTF-8">\n',
        '  <meta name="viewport" content="width=device-width, initial-scale=1">\n',
        "  <title>hotShiny App</title>\n",
        "  <!-- Bootstrap 5 CSS -->\n",
        '  <link rel="stylesheet" href="/static/bootstrap5/bootstrap.min.css">\n',
        "  <!-- hotShiny styles -->\n",
        "  <style>\n",
        "    .shiny-input-container { margin-bottom: 1rem; }\n",
        "    .shiny-text-output { min-height: 1.5em; }\n",
        "    .shiny-plot-output { background: #f8f9fa; border: 1px solid #dee2e6; }\n",
        "    .shiny-plot-output img { max-width: 100%; height: auto; }\n",
        "    .well { background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 0.375rem; padding: 1rem; margin-bottom: 1rem; }\n",
        "    .help-block { color: #6c757d; font-size: 0.875em; margin-top: 0.25rem; }\n",
        "  </style>\n",
        "</head>\n",
        "<body>\n",
        '  <div id="app" class="container-fluid py-3">\n',
        ui_html,
        "  </div>\n",
        "  <!-- Bootstrap 5 JS Bundle -->\n",
        '  <script src="/static/bootstrap5/bootstrap.bundle.min.js"></script>\n',
        "  <!-- hotShiny client scripts -->\n",
        '  <script src="/static/websocket-client.js"></script>\n',
        '  <script src="/static/reactive-client.js"></script>\n',
        '  <script src="/static/dom-diff.js"></script>\n',
        '  <script src="/static/inputs.js"></script>\n',
        '  <script src="/static/hotshiny.js"></script>\n',
        "  <script>\n",
        "    console.log('hotShiny app loaded with Bootstrap 5');\n",
        "  </script>\n",
        "</body>\n",
        "</html>\n"
      )

      html
    },

    # Convert UI object to HTML string (fallback method)
    ui_to_html = function(ui_obj) {
      if (is.null(ui_obj)) {
        return("")
      }

      # Handle raw HTML
      if (inherits(ui_obj, "html")) {
        return(as.character(ui_obj))
      }

      # Handle character strings
      if (is.character(ui_obj) && !inherits(ui_obj, "html")) {
        # Escape HTML entities
        ui_obj <- gsub("&", "&amp;", ui_obj, fixed = TRUE)
        ui_obj <- gsub("<", "&lt;", ui_obj, fixed = TRUE)
        ui_obj <- gsub(">", "&gt;", ui_obj, fixed = TRUE)
        return(ui_obj)
      }

      # Handle tag lists
      if (inherits(ui_obj, "shiny.tag.list")) {
        return(paste(sapply(ui_obj, function(x) self$ui_to_html(x)), collapse = ""))
      }

      if (is.list(ui_obj)) {
        # Check if it's a Shiny tag or our tag structure
        if ("name" %in% names(ui_obj)) {
          tag_name <- ui_obj$name
          attrs <- if ("attribs" %in% names(ui_obj)) ui_obj$attribs else list()
          children <- if ("children" %in% names(ui_obj)) {
            if (is.list(ui_obj$children) && length(ui_obj$children) > 0) {
              ui_obj$children
            } else {
              list()
            }
          } else {
            # Check if there are unnamed list elements
            unnamed <- ui_obj[!names(ui_obj) %in% c("name", "attribs", "children")]
            if (length(unnamed) > 0) {
              unnamed
            } else {
              list()
            }
          }

          attr_str <- ""
          if (length(attrs) > 0 && !is.null(names(attrs))) {
            attr_parts <- character(0)
            for (i in seq_along(attrs)) {
              attr_name <- names(attrs)[i]
              attr_value <- attrs[[i]]
              
              if (!is.null(attr_name) && attr_name != "" && !is.null(attr_value)) {
                # Handle logical attributes
                if (is.logical(attr_value)) {
                  if (isTRUE(attr_value)) {
                    attr_parts <- c(attr_parts, attr_name)
                  }
                } else {
                  # Escape attribute values
                  escaped_value <- gsub('"', "&quot;", as.character(attr_value), fixed = TRUE)
                  attr_parts <- c(attr_parts, paste0(attr_name, '="', escaped_value, '"'))
                }
              }
            }
            if (length(attr_parts) > 0) {
              attr_str <- paste0(" ", paste(attr_parts, collapse = " "))
            }
          }

          children_html <- ""
          if (length(children) > 0) {
            children_html <- paste(sapply(children, function(x) {
              self$ui_to_html(x)
            }), collapse = "")
          }

          # Self-closing (void) tags
          void_elements <- c("area", "base", "br", "col", "embed", "hr", "img", "input",
                            "link", "meta", "param", "source", "track", "wbr")
          
          if (tag_name %in% void_elements) {
            return(paste0("<", tag_name, attr_str, " />"))
          } else {
            return(paste0("<", tag_name, attr_str, ">", children_html, "</", tag_name, ">"))
          }
        } else {
          # Regular list - process each element
          if (length(ui_obj) > 0) {
            return(paste(sapply(ui_obj, function(x) self$ui_to_html(x)), collapse = ""))
          } else {
            return("")
          }
        }
      }

      # Fallback: convert to string
      as.character(ui_obj)
    },

    # Get graph
    get_graph = function() {
      self$builder$get_graph()
    },

    # Get executor
    get_executor = function() {
      self$executor
    },

    # Get state manager
    get_state_manager = function() {
      self$state_manager
    }
  )
)
