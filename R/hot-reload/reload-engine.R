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
        
        # Update app
        new_graph <- new_builder$get_graph()
        self$app$builder <- new_builder
        self$app$executor$graph <- new_graph
        self$app$executor$builder <- new_builder
        
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
