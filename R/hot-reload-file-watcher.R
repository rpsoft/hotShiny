# File Watcher
# Monitors source files for changes

#' File Watcher
#'
#' Watches files for changes and triggers callbacks
FileWatcher <- R6::R6Class("FileWatcher",
  public = list(
    watched_files = NULL,
    callbacks = NULL,
    running = NULL,
    check_interval = NULL,
    file_mtimes = NULL,
    
    initialize = function(check_interval = 0.5) {
      self$watched_files <- list()
      self$callbacks <- list()
      self$running <- FALSE
      self$check_interval <- check_interval
      self$file_mtimes <- new.env(parent = emptyenv())
    },
    
    # Watch a file
    watch = function(file_path, callback = NULL) {
      if (!file.exists(file_path)) {
        warning("File does not exist: ", file_path)
        return(invisible(NULL))
      }
      
      abs_path <- normalizePath(file_path)
      log_debug("[FileWatcher] watch() called for:", abs_path, "callback:", !is.null(callback), "\n", file = stderr())
      
      # Store file
      self$watched_files <- c(self$watched_files, list(abs_path))
      
      # Store modification time
      mtime <- file.mtime(abs_path)
      assign(abs_path, mtime, envir = self$file_mtimes)
      log_debug("[FileWatcher] Stored mtime for", abs_path, ":", mtime, "\n", file = stderr())
      
      # Store callback
      if (!is.null(callback)) {
        self$callbacks[[abs_path]] <- callback
        log_debug("[FileWatcher] Stored callback for", abs_path, "\n", file = stderr())
      } else {
        log_debug("[FileWatcher] WARNING: No callback provided for", abs_path, "\n", file = stderr())
      }
      
      invisible(NULL)
    },
    
    # Watch a directory
    watch_directory = function(dir_path, pattern = "\\.R$", callback = NULL, recursive = TRUE) {
      if (!dir.exists(dir_path)) {
        warning("Directory does not exist: ", dir_path)
        return(invisible(NULL))
      }
      
      abs_dir <- normalizePath(dir_path)
      log_debug("[FileWatcher] Watching directory:", abs_dir, "pattern:", pattern, "recursive:", recursive, "\n", file = stderr())
      
      # Find all matching files
      files <- list.files(
        abs_dir,
        pattern = pattern,
        full.names = TRUE,
        recursive = recursive
      )
      
      log_debug("[FileWatcher] Found", length(files), "files matching pattern\n", file = stderr())
      
      # Watch each file
      for (file in files) {
        log_debug("[FileWatcher] Watching file:", file, "\n", file = stderr())
        self$watch(file, callback)
      }
      
      invisible(NULL)
    },
    
    # Start watching
    start = function() {
      if (self$running) {
        return(invisible(NULL))
      }
      
      self$running <- TRUE
      self$check_loop()
    },
    
    # Stop watching
    stop = function() {
      self$running <- FALSE
    },
    
    # Check loop (runs asynchronously)
    check_loop = function() {
      if (!self$running) {
        return(invisible(NULL))
      }
      
      # Check all watched files
      for (file_path in self$watched_files) {
        if (!file.exists(file_path)) {
          next
        }
        
        current_mtime <- file.mtime(file_path)
        stored_mtime <- get(file_path, envir = self$file_mtimes, inherits = FALSE)
        
        if (current_mtime > stored_mtime) {
          # File changed!
          log_debug("[FileWatcher] File changed detected:", file_path, "\n", file = stderr())
          log_debug("[FileWatcher] Old mtime:", stored_mtime, ", New mtime:", current_mtime, "\n", file = stderr())
          assign(file_path, current_mtime, envir = self$file_mtimes)
          
          # Call callback
          callback <- self$callbacks[[file_path]]
          if (!is.null(callback)) {
            log_debug("[FileWatcher] Calling callback for:", file_path, "\n", file = stderr())
            tryCatch({
              callback(file_path)
            }, error = function(e) {
              warning("Error in file watcher callback: ", conditionMessage(e))
              log_debug("[FileWatcher] ERROR in callback:", conditionMessage(e), "\n", file = stderr())
            })
          } else {
            log_debug("[FileWatcher] WARNING: No callback found for:", file_path, "\n", file = stderr())
          }
        }
      }
      
      # Schedule next check
      later::later(function() {
        self$check_loop()
      }, delay = self$check_interval)
    },
    
    # Get watched files
    get_watched_files = function() {
      self$watched_files
    },
    
    # Remove a file from watch list
    unwatch = function(file_path) {
      abs_path <- normalizePath(file_path)
      self$watched_files <- setdiff(self$watched_files, list(abs_path))
      self$callbacks[[abs_path]] <- NULL
      if (exists(abs_path, envir = self$file_mtimes)) {
        rm(list = abs_path, envir = self$file_mtimes)
      }
    }
  )
)

#' Create a file watcher
#'
#' @param check_interval Interval between checks (seconds)
#' @return FileWatcher instance
create_file_watcher <- function(check_interval = 0.5) {
  FileWatcher$new(check_interval = check_interval)
}
