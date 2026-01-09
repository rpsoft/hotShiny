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
          self$file_watcher$watch_directory(path, callback = function(file) {
            self$handle_file_change(file)
          })
        } else if (file.exists(path)) {
          self$file_watcher$watch(path, callback = function(file) {
            self$handle_file_change(file)
          })
        }
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
        return(invisible(NULL))
      }

      message("File changed: ", file_path)

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
          sys.source(file_path, envir = temp_env)

          # Check if we captured ui/server via app() spy
          if (exists("captured_ui", envir = temp_env) && exists("captured_server", envir = temp_env)) {
            message("Confirming hot reload: found updated app() definition")
            updated_ui <- get("captured_ui", envir = temp_env)
            updated_server <- get("captured_server", envir = temp_env)

            # Update current app components
            self$app$ui <- updated_ui
            self$app$server_func <- updated_server
          } else {
            # Fallback: check if 'ui' and 'server' are defined as variables
            # This handles cases where app() isn't called or names are standard
            if (exists("server", envir = temp_env) && is.function(temp_env$server)) {
              self$app$server_func <- temp_env$server
              message("Hot reload: updated server function from variable")
            }
            if (exists("ui", envir = temp_env)) { # ui can be function or tag
              self$app$ui <- temp_env$ui
              message("Hot reload: updated ui from variable")
            }
          }
        },
        error = function(e) {
          warning("Hot update extraction failed for ", file_path, ": ", conditionMessage(e))
          # Continue anyway, maybe it was just a helper file change
        }
      )

      message("Reloading...")

      # Perform hot reload
      tryCatch(
        {
          diff_summary <- self$reload()

          # Notify clients
          if (!is.null(self$app$ws_server)) {
            self$app$ws_server$send_hot_reload(diff_summary)
          }

          message("Reload complete")
        },
        error = function(e) {
          warning("Hot reload failed: ", conditionMessage(e))
        }
      )
    },

    # Perform hot reload
    reload = function() {
      tryCatch({
        message("[HotReload] Starting reload...")
        
        # Load required classes from namespace
        ns <- asNamespace("hotShiny")
        GraphBuilderClass <- get("GraphBuilder", envir = ns)
        InputProxyClass <- get("InputProxy", envir = ns)
        OutputProxyFn <- get("OutputProxy", envir = ns)
        set_graph_builder_fn <- get("set_graph_builder", envir = ns)
        
        # Create new builder
        message("[HotReload] Creating new builder...")
        new_builder <- GraphBuilderClass$new()
        set_graph_builder_fn(new_builder)
        
        # Re-execute server
        message("[HotReload] Re-executing server...")
        input <- InputProxyClass$new(builder = new_builder, executor = self$app$executor)
        output <- OutputProxyFn(new_builder)
        
        if (is.function(self$app$server_func)) {
          self$app$server_func(input, output, NULL)
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
        
        # CRITICAL: Execute all nodes to compute new values after reload
        message("[HotReload] Executing nodes to compute new values...")
        self$app$executor$execute()
        
        # CRITICAL: Send updated output values to clients
        message("[HotReload] Sending output updates to clients...")
        self$app$executor$send_output_updates()
        
        message("[HotReload] Complete! Nodes: ", length(new_graph$get_all_nodes()))
        list(status = "success", nodes = length(new_graph$get_all_nodes()))
      }, error = function(e) {
        warning("[HotReload] Error: ", conditionMessage(e))
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
