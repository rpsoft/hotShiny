# Hot Reload Engine
# Orchestrates hot reload process

#' Hot Reload Engine
#'
#' Manages hot reload process
HotReloadEngine <- R6::R6Class("HotReloadEngine",
  public = list(
    app = NULL,
    file_watcher = NULL,
    version_manager = NULL,
    preservation_manager = NULL,
    enabled = NULL,
    initialize = function(app) {
      self$app <- app
      # Get classes - load them if needed
      ns <- asNamespace("hotShiny")
      base_path <- getwd()
      if (!file.exists(file.path(base_path, "R"))) {
        base_path <- NULL
      }
      load_env <- new.env(parent = ns)

      # Load FileWatcher
      if (!is.null(base_path)) {
        file_path <- file.path(base_path, "R/hot-reload/file-watcher.R")
        if (file.exists(file_path)) {
          sys.source(file_path, envir = load_env)
        }
      }
      FileWatcherClass <- if (exists("FileWatcher", envir = load_env)) {
        get("FileWatcher", envir = load_env)
      } else if (exists("FileWatcher", envir = ns)) {
        get("FileWatcher", envir = ns)
      } else {
        stop("FileWatcher class not found")
      }

      # Load GraphVersionManager
      if (!is.null(base_path)) {
        file_path <- file.path(base_path, "R/runtime/versioning.R")
        if (file.exists(file_path)) {
          sys.source(file_path, envir = load_env)
        }
      }
      GraphVersionManagerClass <- if (exists("GraphVersionManager", envir = load_env)) {
        get("GraphVersionManager", envir = load_env)
      } else if (exists("GraphVersionManager", envir = ns)) {
        get("GraphVersionManager", envir = ns)
      } else {
        stop("GraphVersionManager class not found")
      }

      # Load StatePreservationManager
      if (!is.null(base_path)) {
        file_path <- file.path(base_path, "R/hot-reload/state-preservation.R")
        if (file.exists(file_path)) {
          sys.source(file_path, envir = load_env)
        }
      }
      StatePreservationManagerClass <- if (exists("StatePreservationManager", envir = load_env)) {
        get("StatePreservationManager", envir = load_env)
      } else if (exists("StatePreservationManager", envir = ns)) {
        get("StatePreservationManager", envir = ns)
      } else {
        stop("StatePreservationManager class not found")
      }

      self$file_watcher <- FileWatcherClass$new()
      self$version_manager <- GraphVersionManagerClass$new()
      self$preservation_manager <- StatePreservationManagerClass$new()
      self$enabled <- FALSE
    },

    # Enable hot reload
    enable = function(watch_paths = NULL) {
      if (self$enabled) {
        return(invisible(NULL))
      }

      self$enabled <- TRUE

      # compute_graph_hash will be loaded when versioning.R loads serializer.R

      # Save initial graph version
      graph <- self$app$get_graph()
      self$version_manager$save_version(graph, metadata = list(initial = TRUE))

      # Set up file watcher
      if (is.null(watch_paths)) {
        # Default: watch current directory
        watch_paths <- getwd()
      }

      # Watch files
      for (path in watch_paths) {
        if (dir.exists(path)) {
          cat("[HotReload] Watching directory:", path, "\n", file = stderr())
          self$file_watcher$watch_directory(path, callback = function(file) {
            cat("[HotReload] Directory watch callback triggered for:", file, "\n", file = stderr())
            self$handle_file_change(file)
          })
        } else if (file.exists(path)) {
          cat("[HotReload] Watching file:", path, "\n", file = stderr())
          self$file_watcher$watch(path, callback = function(file) {
            cat("[HotReload] File watch callback triggered for:", file, "\n", file = stderr())
            self$handle_file_change(file)
          })
        } else {
          cat("[HotReload] WARNING: Path does not exist:", path, "\n", file = stderr())
        }
      }
      
      # Log watched files
      watched <- self$file_watcher$get_watched_files()
      cat("[HotReload] Total watched files:", length(watched), "\n", file = stderr())
      if (length(watched) > 0) {
        cat("[HotReload] Watched files:", paste(watched, collapse = ", "), "\n", file = stderr())
      }

      # Start file watcher
      self$file_watcher$start()

      message("Hot reload enabled")
    },

    # Disable hot reload
    disable = function() {
      if (!self$enabled) {
        return(invisible(NULL))
      }

      self$enabled <- FALSE
      self$file_watcher$stop()

      message("Hot reload disabled")
    },

    # Handle file change
    handle_file_change = function(file_path) {
      if (!self$enabled) {
        cat("[HotReload] handle_file_change: Hot reload is disabled, ignoring\n", file = stderr())
        return(invisible(NULL))
      }

      message("[HotReload] File changed: ", file_path)
      cat("[HotReload] handle_file_change: Processing file change for:", file_path, "\n", file = stderr())

      # Attempt to extract updated app components from the file
      tryCatch(
        {
          # Create a temporary environment for sourcing the file
          # Use globalenv as parent so it can find libraries etc.
          temp_env <- new.env(parent = globalenv())

          # Spy on app() creation
          # We define a mock 'app' function that captures ui and server
          # This intercepts the app creation in standard hotShiny scripts
          temp_env$app <- function(ui, server, ...) {
            # Capture them in the environment
            assign("captured_ui", ui, envir = temp_env)
            assign("captured_server", server, envir = temp_env)

            # Return a dummy object that swallows runApp
            # This prevents the app from trying to start a second server on the same port
            structure(list(
              runApp = function(...) {
                message("Hot reload: Skipping app startup in file source")
              }
            ), class = "HotShinyAppStub")
          }

          # Mock enable_hot_reload to prevent errors/recursion
          temp_env$enable_hot_reload <- function(...) {}

          # Source the file to execute definitions
          cat("[HotReload] Sourcing file:", file_path, "\n", file = stderr())
          sys.source(file_path, envir = temp_env)
          cat("[HotReload] File sourced successfully\n", file = stderr())
          
          # List all variables in temp_env for debugging
          temp_vars <- ls(envir = temp_env, all.names = TRUE)
          cat("[HotReload] Variables in temp_env after sourcing:", paste(temp_vars, collapse = ", "), "\n", file = stderr())

          # Check if we captured ui/server via app() spy
          if (exists("captured_ui", envir = temp_env) && exists("captured_server", envir = temp_env)) {
            message("[HotReload] Confirming hot reload: found updated app() definition")
            cat("[HotReload] Found captured_ui and captured_server\n", file = stderr())
            updated_ui <- get("captured_ui", envir = temp_env)
            updated_server <- get("captured_server", envir = temp_env)
            
            cat("[HotReload] updated_server is function:", is.function(updated_server), "\n", file = stderr())
            cat("[HotReload] updated_ui type:", class(updated_ui), "\n", file = stderr())

            # Update current app components
            self$app$ui <- updated_ui
            self$app$server_func <- updated_server
            cat("[HotReload] Updated app$ui and app$server_func\n", file = stderr())
            cat("[HotReload] app$server_func is now function:", is.function(self$app$server_func), "\n", file = stderr())
          } else {
            # Fallback: check if 'ui' and 'server' are defined as variables
            # This handles cases where app() isn't called or names are standard
            cat("[HotReload] Checking for ui and server variables in temp_env\n", file = stderr())
            cat("[HotReload] Variables in temp_env:", paste(ls(envir = temp_env), collapse = ", "), "\n", file = stderr())
            if (exists("server", envir = temp_env) && is.function(temp_env$server)) {
              self$app$server_func <- temp_env$server
              message("[HotReload] Updated server function from variable")
              cat("[HotReload] Updated server function from variable\n", file = stderr())
              cat("[HotReload] app$server_func is now function:", is.function(self$app$server_func), "\n", file = stderr())
            } else {
              cat("[HotReload] Server variable not found or not a function\n", file = stderr())
              if (exists("server", envir = temp_env)) {
                cat("[HotReload] Server exists but is not a function, type:", class(temp_env$server), "\n", file = stderr())
              }
            }
            if (exists("ui", envir = temp_env)) { # ui can be function or tag
              self$app$ui <- temp_env$ui
              message("[HotReload] Updated ui from variable")
              cat("[HotReload] Updated ui from variable\n", file = stderr())
            } else {
              cat("[HotReload] UI variable not found\n", file = stderr())
            }
          }
        },
        error = function(e) {
          warning("[HotReload] Hot update extraction failed for ", file_path, ": ", conditionMessage(e))
          cat("[HotReload] ERROR in handle_file_change:", conditionMessage(e), "\n", file = stderr())
          cat("[HotReload] Error traceback:\n", file = stderr())
          print(traceback())
          # Continue anyway, maybe it was just a helper file change
        }
      )

      message("[HotReload] Reloading...")
      cat("[HotReload] Starting reload process\n", file = stderr())
      cat("[HotReload] app$server_func is function:", is.function(self$app$server_func), "\n", file = stderr())
      if (!is.function(self$app$server_func)) {
        cat("[HotReload] ERROR: app$server_func is not a function! Cannot reload.\n", file = stderr())
        return(invisible(NULL))
      }

      # Perform hot reload
      tryCatch(
        {
          diff_summary <- self$reload()

          # Notify clients
          if (!is.null(self$app$ws_server)) {
            self$app$ws_server$send_hot_reload(diff_summary)
            cat("[HotReload] Sent hot reload notification to clients\n", file = stderr())
          } else {
            cat("[HotReload] WARNING: ws_server is NULL, cannot notify clients\n", file = stderr())
          }

          message("[HotReload] Reload complete")
          cat("[HotReload] Reload complete\n", file = stderr())
        },
        error = function(e) {
          warning("[HotReload] Hot reload failed: ", conditionMessage(e))
          cat("[HotReload] ERROR: Hot reload failed: ", conditionMessage(e), "\n", file = stderr())
        }
      )
    },

    # Perform hot reload
    reload = function() {
      tryCatch({
        message("[HotReload] Starting reload...")
        cat("[HotReload] reload() function called\n", file = stderr())
        
        # First, set up base_path and load_env
        ns <- asNamespace("hotShiny")
        base_path <- getwd()
        if (!file.exists(file.path(base_path, "R"))) {
          base_path <- system.file(package = "hotShiny")
        }
        cat("[HotReload] base_path:", base_path, "\n", file = stderr())
        
        load_env <- new.env(parent = ns)
        
        # Load required R files to get classes
        files_to_load <- c(
          "R/ir/node-types.R",
          "R/ir/graph-builder.R",
          "R/ir/dependency-tracker.R",
          "R/ir/serializer.R",
          "R/core/reactive.R",
          "R/core/observe.R",
          "R/core/render.R",
          "R/core/values.R"
        )
        for (file_rel in files_to_load) {
          file_path <- file.path(base_path, file_rel)
          if (file.exists(file_path)) {
            cat("[HotReload] Loading:", file_rel, "\n", file = stderr())
            sys.source(file_path, envir = load_env)
          }
        }
        
        # Get required classes from load_env
        GraphBuilderClass <- if (exists("GraphBuilder", envir = load_env)) {
          get("GraphBuilder", envir = load_env)
        } else {
          stop("GraphBuilder not found after loading files")
        }
        cat("[HotReload] Got GraphBuilder class\n", file = stderr())
        
        InputProxyClass <- if (exists("InputProxy", envir = load_env)) {
          get("InputProxy", envir = load_env)
        } else {
          stop("InputProxy not found after loading files")
        }
        cat("[HotReload] Got InputProxy class\n", file = stderr())
        
        OutputProxyFn <- if (exists("OutputProxy", envir = load_env)) {
          get("OutputProxy", envir = load_env)
        } else {
          stop("OutputProxy not found after loading files")
        }
        cat("[HotReload] Got OutputProxy function\n", file = stderr())
        
        set_graph_builder_fn <- if (exists("set_graph_builder", envir = load_env)) {
          get("set_graph_builder", envir = load_env)
        } else {
          stop("set_graph_builder not found after loading files")
        }
        cat("[HotReload] Got set_graph_builder function\n", file = stderr())
        
        # Create new builder
        message("[HotReload] Creating new builder...")
        cat("[HotReload] Creating new builder...\n", file = stderr())
        new_builder <- GraphBuilderClass$new()
        set_graph_builder_fn(new_builder)
        
        # CRITICAL: Set up execution environment with helper functions (same as in runApp)
        message("[HotReload] Setting up execution environment...")
        exec_env <- new.env(parent = parent.frame())
        
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
          base_path <- system.file(package = "hotShiny")
        }
        
        # Load core files to get functions (same order as runApp)
        if (!is.null(base_path)) {
          # Load dependency tracker first (needed by reactive, observe, render)
          dep_tracker_file <- file.path(base_path, "R/ir/dependency-tracker.R")
          if (file.exists(dep_tracker_file)) {
            sys.source(dep_tracker_file, envir = load_env)
          }
          
          # Then load core files
          core_files <- c(
            "R/core/reactive.R",
            "R/core/observe.R",
            "R/core/render.R",
            "R/core/values.R"
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
          get("set_graph_builder", envir = exec_env)(new_builder)
        } else if (exists("set_graph_builder", envir = ns)) {
          get("set_graph_builder", envir = ns)(new_builder)
        }
        
        # Set executor in execution environment
        if (exists("set_executor", envir = exec_env)) {
          get("set_executor", envir = exec_env)(self$app$executor)
        } else if (exists("set_executor", envir = ns)) {
          get("set_executor", envir = ns)(self$app$executor)
        }
        
        # Get InputProxy and OutputProxy classes
        InputProxyClass <- if (exists("InputProxy", envir = load_env)) {
          get("InputProxy", envir = load_env)
        } else {
          tryCatch(get("InputProxy", envir = ns), error = function(e) NULL)
        }
        OutputProxyFn <- if (exists("OutputProxy", envir = load_env)) {
          get("OutputProxy", envir = load_env)
        } else {
          tryCatch(get("OutputProxy", envir = ns), error = function(e) NULL)
        }
        
        if (is.null(InputProxyClass) || is.null(OutputProxyFn)) {
          # Load values.R to get proxy classes
          if (!is.null(base_path)) {
            values_file <- file.path(base_path, "R/core/values.R")
            if (file.exists(values_file)) {
              sys.source(values_file, envir = load_env)
              if (is.null(InputProxyClass) && exists("InputProxy", envir = load_env)) {
                InputProxyClass <- get("InputProxy", envir = load_env)
              }
              if (is.null(OutputProxyFn) && exists("OutputProxy", envir = load_env)) {
                OutputProxyFn <- get("OutputProxy", envir = load_env)
              }
            }
          }
        }
        
        # Create input and output proxies
        input <- InputProxyClass$new(builder = new_builder, executor = self$app$executor)
        if (is.function(OutputProxyFn)) {
          output <- OutputProxyFn(new_builder)
        } else {
          output <- OutputProxyFn$new(builder = new_builder)
        }
        session <- NULL
        
        # Re-execute server with proper environment setup
        message("[HotReload] Re-executing server function...")
        cat("[HotReload] About to execute server_func, is function:", is.function(self$app$server_func), "\n", file = stderr())
        if (is.function(self$app$server_func)) {
          cat("[HotReload] Executing server_func now...\n", file = stderr())
          # Attach functions to server function's environment
          server_env <- environment(self$app$server_func)
          if (is.null(server_env)) {
            server_env <- exec_env
          }
          # Copy functions to server environment
          for (func_name in funcs_to_copy) {
            if (exists(func_name, envir = exec_env)) {
              assign(func_name, get(func_name, envir = exec_env), envir = server_env)
            }
          }
          # Also copy extract_dependencies and ast_to_expr
          for (func_name in c("extract_dependencies", "ast_to_expr")) {
            if (exists(func_name, envir = exec_env)) {
              assign(func_name, get(func_name, envir = exec_env), envir = server_env)
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
          
          # Execute server function
          self$app$server_func(input, output, session)
        } else {
          warning("[HotReload] server_func is not a function!")
        }
        
        # CRITICAL: Register reactive expressions by variable name (same as in runApp)
        # Scan environments for ReactiveProxy objects
        message("[HotReload] Scanning environments for reactive sources...")
        envs_to_scan <- list()
        
        # Scan environments captured in graph nodes
        graph_nodes <- new_builder$get_graph()$get_all_nodes()
        for (node in graph_nodes) {
          if (!is.null(node$env) && is.environment(node$env)) {
            envs_to_scan <- c(envs_to_scan, list(node$env))
          }
        }
        
        # Scan all environments for ReactiveProxy objects
        for (scan_env in envs_to_scan) {
          tryCatch(
            {
              env_vars <- ls(envir = scan_env, all.names = TRUE)
              for (var_name in env_vars) {
                tryCatch(
                  {
                    var_value <- get(var_name, envir = scan_env)
                    if (inherits(var_value, "ReactiveProxy") && !is.null(var_value$node_id)) {
                      # Register this reactive by its variable name
                      if (!is.null(new_builder$reactive_context)) {
                        if (!exists("reactive_sources", envir = new_builder$reactive_context)) {
                          assign("reactive_sources", new.env(parent = emptyenv()), envir = new_builder$reactive_context)
                        }
                        reactive_sources <- get("reactive_sources", envir = new_builder$reactive_context)
                        # Only register if not already registered (avoid duplicates)
                        if (!exists(var_name, envir = reactive_sources, inherits = FALSE)) {
                          assign(var_name, var_value$node_id, envir = reactive_sources)
                          message("[HotReload] Registered reactive source: ", var_name, " -> ", var_value$node_id)
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
            }
          )
        }
        
        # CRITICAL: Manually trigger set_output_name for any pending RenderProxies (same as in runApp)
        message("[HotReload] Triggering set_output_name for pending RenderProxies...")
        if (is.environment(output)) {
          output_names <- ls(envir = output, all.names = TRUE)
          for (name in output_names) {
            val <- get(name, envir = output)
            if (inherits(val, "RenderProxy") && isTRUE(val$pending_output_name)) {
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
        
        # CRITICAL: Re-extract dependencies after server execution (same as in runApp)
        # This ensures reactive sources are registered before dependency extraction
        message("[HotReload] Re-extracting dependencies...")
        graph <- new_builder$get_graph()
        all_nodes <- graph$get_all_nodes()
        
        # Load helper functions
        ns <- asNamespace("hotShiny")
        base_path <- getwd()
        if (!file.exists(file.path(base_path, "R"))) {
          base_path <- system.file(package = "hotShiny")
        }
        load_env <- new.env(parent = ns)
        dep_file <- file.path(base_path, "R/ir/dependency-tracker.R")
        if (file.exists(dep_file)) {
          sys.source(dep_file, envir = load_env)
        }
        extract_deps_fn <- if (exists("extract_dependencies", envir = load_env)) {
          get("extract_dependencies", envir = load_env)
        } else if (exists("extract_dependencies", envir = ns)) {
          get("extract_dependencies", envir = ns)
        } else {
          stop("extract_dependencies function not found")
        }
        
        serializer_file <- file.path(base_path, "R/ir/serializer.R")
        if (file.exists(serializer_file)) {
          sys.source(serializer_file, envir = load_env)
        }
        ast_to_expr_fn <- if (exists("ast_to_expr", envir = load_env)) {
          get("ast_to_expr", envir = load_env)
        } else if (exists("ast_to_expr", envir = ns)) {
          get("ast_to_expr", envir = ns)
        } else {
          stop("ast_to_expr function not found")
        }
        
        # Re-extract dependencies for render nodes
        for (node in all_nodes) {
          if (inherits(node, "RenderNode") && !is.null(node$expr)) {
            expr <- ast_to_expr_fn(node$expr)
            new_deps <- extract_deps_fn(expr)
            node$deps <- new_deps
            # Rebuild edges
            graph$edges <- Filter(function(e) e$to != node$id, graph$edges)
            for (dep_id in new_deps) {
              graph$edges <- c(graph$edges, list(list(from = dep_id, to = node$id)))
            }
          }
        }
        
        # Update app
        new_graph <- new_builder$get_graph()
        self$app$builder <- new_builder
        self$app$executor$graph <- new_graph
        self$app$executor$builder <- new_builder
        
        # CRITICAL: Preserve input values from old state_manager
        message("[HotReload] Preserving input values...")
        cat("[HotReload] Preserving input values from state_manager\n", file = stderr())
        state_manager <- self$app$state_manager
        if (!is.null(state_manager)) {
          # Get all values from state manager using serialize_state
          old_values <- state_manager$serialize_state()
          cat("[HotReload] Found", length(old_values), "values in state_manager\n", file = stderr())
          
          # Copy input values to ensure they're available
          for (node_id in names(old_values)) {
            if (grepl("^input\\.", node_id)) {
              value <- old_values[[node_id]]
              cat("[HotReload] Preserving input value:", node_id, "=", value, "\n", file = stderr())
              # Ensure the input node exists in the new graph
              existing_node <- tryCatch(new_graph$get_node(node_id), error = function(e) NULL)
              if (is.null(existing_node)) {
                cat("[HotReload] Creating input node:", node_id, "\n", file = stderr())
                # Register input in new builder
                input_name <- sub("^input\\.", "", node_id)
                new_builder$register_input(input_name)
              }
              # Set the value in state manager (already there, but make sure)
              state_manager$set_value(node_id, value)
            }
          }
        }
        
        # Also mark all nodes as dirty so they re-execute with the new logic
        cat("[HotReload] Marking all nodes as dirty\n", file = stderr())
        all_new_nodes <- new_graph$get_all_nodes()
        for (node in all_new_nodes) {
          if (!is.null(node$id)) {
            state_manager$mark_dirty(node$id)
          }
        }
        
        # CRITICAL: Execute all nodes to compute new values after reload
        message("[HotReload] Executing nodes to compute new values...")
        cat("[HotReload] Calling executor$execute()\n", file = stderr())
        self$app$executor$execute()
        
        # CRITICAL: Send updated output values to clients
        message("[HotReload] Sending output updates to clients...")
        self$app$executor$send_output_updates()
        
        message("[HotReload] Complete! Nodes: ", length(new_graph$get_all_nodes()))
        list(status = "success", nodes = length(new_graph$get_all_nodes()))
      }, error = function(e) {
        warning("[HotReload] Error in reload(): ", conditionMessage(e))
        cat("[HotReload] ERROR in reload():", conditionMessage(e), "\n", file = stderr())
        cat("[HotReload] Error call:", paste(deparse(e$call), collapse = " "), "\n", file = stderr())
        traceback()
        list(status = "error", message = conditionMessage(e))
      })
    },

    # Get reload status
    get_status = function() {
      list(
        enabled = self$enabled,
        watched_files = self$file_watcher$get_watched_files(),
        versions = length(self$version_manager$versions),
        current_version = self$version_manager$current_version - 1L
      )
    }
  )
)

#' Enable hot reload for an app
#'
#' @param app HotShinyApp instance
#' @param watch_paths Paths to watch (defaults to current directory)
enable_hot_reload <- function(app, watch_paths = NULL) {
  if (!inherits(app, "HotShinyApp")) {
    stop("app must be a HotShinyApp instance")
  }

  # Load required classes if not available
  ns <- asNamespace("hotShiny")
  base_path <- getwd()
  if (!file.exists(file.path(base_path, "R"))) {
    base_path <- NULL
  }

  load_env <- new.env(parent = ns)
  if (!is.null(base_path)) {
    # Load required files in order
    files_to_load <- c(
      "R/hot-reload/file-watcher.R",
      "R/runtime/versioning.R",
      "R/hot-reload/state-preservation.R",
      "R/hot-reload/graph-diff.R",
      "R/hot-reload/reload-engine.R"
    )
    for (file_rel in files_to_load) {
      file_path <- file.path(base_path, file_rel)
      if (file.exists(file_path)) {
        sys.source(file_path, envir = load_env)
      }
    }
  }

  # Get HotReloadEngine class
  HotReloadEngineClass <- if (exists("HotReloadEngine", envir = load_env)) {
    get("HotReloadEngine", envir = load_env)
  } else if (exists("HotReloadEngine", envir = ns, inherits = FALSE)) {
    get("HotReloadEngine", envir = ns, inherits = FALSE)
  } else {
    stop("HotReloadEngine class not found")
  }

  # Create or get reload engine
  # Note: app$reload_engine might not be assignable if environment is locked
  # So we'll store it in a different way
  if (is.null(attr(app, "reload_engine"))) {
    attr(app, "reload_engine") <- HotReloadEngineClass$new(app)
  }

  reload_engine <- attr(app, "reload_engine")
  reload_engine$enable(watch_paths = watch_paths)
  app
}
