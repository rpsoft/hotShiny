# Session Manager
# Manages client sessions

#' Session
#'
#' Represents a client session
Session <- R6::R6Class("Session",
  public = list(
    id = NULL,
    ws = NULL,
    created_at = NULL,
    last_activity = NULL,
    
    initialize = function(id, ws) {
      self$id <- id
      self$ws <- ws
      self$created_at <- Sys.time()
      self$last_activity <- Sys.time()
    },
    
    # Update activity
    update_activity = function() {
      self$last_activity <- Sys.time()
    }
  )
)

#' Session Manager
#'
#' Manages client sessions
SessionManager <- R6::R6Class("SessionManager",
  public = list(
    sessions = NULL,
    
    initialize = function() {
      self$sessions <- new.env(parent = emptyenv())
    },
    
    # Create new session
    create_session = function(ws) {
      session_id <- paste0("session_", length(ls(envir = self$sessions)) + 1L)
      session <- Session$new(session_id, ws)
      assign(session_id, session, envir = self$sessions)
      session
    },
    
    # Get session
    get_session = function(session_id) {
      if (exists(session_id, envir = self$sessions)) {
        get(session_id, envir = self$sessions)
      } else {
        NULL
      }
    },
    
    # Remove session
    remove_session = function(session_id) {
      if (exists(session_id, envir = self$sessions)) {
        rm(list = session_id, envir = self$sessions)
      }
    },
    
    # Get all sessions
    get_all_sessions = function() {
      as.list(self$sessions)
    },
    
    # Clean up inactive sessions
    cleanup_inactive = function(timeout_seconds = 3600) {
      now <- Sys.time()
      to_remove <- list()
      
      for (session_id in ls(envir = self$sessions)) {
        session <- get(session_id, envir = self$sessions)
        inactive_time <- as.numeric(now - session$last_activity, units = "secs")
        
        if (inactive_time > timeout_seconds) {
          to_remove <- c(to_remove, list(session_id))
        }
      }
      
      for (session_id in to_remove) {
        self$remove_session(session_id)
      }
      
      length(to_remove)
    }
  )
)

#' Create session manager
#'
#' @return SessionManager instance
create_session_manager <- function() {
  SessionManager$new()
}
