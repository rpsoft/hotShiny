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
      message("Reloading...")
      
      # Perform hot reload
      tryCatch({
        self$reload()
        message("Reload complete")
      }, error = function(e) {
        warning("Hot reload failed: ", conditionMessage(e))
      })
    },
    
    # Perform hot reload
    reload = function() {
      # Pause execution
      # (In a real implementation, this would pause the executor)
      
      # Get current graph
      old_graph <- self$app$get_graph()
      
      # Save current state
      state_manager <- self$app$get_state_manager()
      
      # Rebuild graph
      # This would involve re-executing the server function
      # For now, we'll create a new builder
      new_builder <- GraphBuilder$new()
      set_graph_builder(new_builder)
      
      # Re-execute server function
      # (This is simplified - real implementation would be more complex)
      tryCatch({
        input <- InputProxy$new(builder = new_builder)
        output <- OutputProxy$new(builder = new_builder)
        session <- NULL
        
        if (is.function(self$app$server_func)) {
          self$app$server_func(input, output, session)
        }
      }, finally = {
        # Clean up
      })
      
      new_graph <- new_builder$get_graph()
      
      # Diff graphs
      diff <- diff_graphs(old_graph, new_graph)
      
      # Preserve state for unchanged nodes
      preserve_unchanged_state(diff, state_manager, self$preservation_manager)
      
      # Migrate state for modified nodes
      migrate_modified_state(diff, old_graph, new_graph, state_manager, self$preservation_manager)
      
      # Update app with new graph
      self$app$builder <- new_builder
      self$app$executor$graph <- new_graph
      
      # Restore preserved state
      restore_unchanged_state(diff, state_manager, self$preservation_manager)
      
      # Save new version
      self$version_manager$save_version(new_graph, metadata = list(
        diff_summary = diff$summary()
      ))
      
      # Resume execution
      # (In a real implementation, this would resume the executor)
      
      # Return diff summary
      diff$summary()
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
