# State Manager
# Manages application state and reactive values

#' State Manager
#'
#' Manages reactive values and application state
StateManager <- R6::R6Class("StateManager",
  public = list(
    values = NULL,  # Store reactive values (node_id -> value)
    dirty_nodes = NULL,  # Set of dirty node IDs
    execution_state = NULL,  # Track execution state
    
    initialize = function() {
      self$values <- new.env(parent = emptyenv())
      self$dirty_nodes <- new.env(parent = emptyenv())
      self$execution_state <- new.env(parent = emptyenv())
    },
    
    # Set a value for a node
    set_value = function(node_id, value) {
      assign(node_id, value, envir = self$values)
      self$mark_dirty(node_id)
    },
    
    # Get a value for a node
    get_value = function(node_id) {
      if (exists(node_id, envir = self$values)) {
        get(node_id, envir = self$values)
      } else {
        NULL
      }
    },
    
    # Mark a node as dirty
    mark_dirty = function(node_id) {
      assign(node_id, TRUE, envir = self$dirty_nodes)
    },
    
    # Check if node is dirty
    is_dirty = function(node_id) {
      exists(node_id, envir = self$dirty_nodes)
    },
    
    # Clear dirty flag
    clear_dirty = function(node_id) {
      if (exists(node_id, envir = self$dirty_nodes)) {
        rm(list = node_id, envir = self$dirty_nodes)
      }
    },
    
    # Clear all dirty flags
    clear_all_dirty = function() {
      self$dirty_nodes <- new.env(parent = emptyenv())
    },
    
    # Get all dirty nodes
    get_dirty_nodes = function() {
      ls(envir = self$dirty_nodes)
    },
    
    # Set execution state
    set_execution_state = function(node_id, state) {
      assign(node_id, state, envir = self$execution_state)
    },
    
    # Get execution state
    get_execution_state = function(node_id) {
      if (exists(node_id, envir = self$execution_state)) {
        get(node_id, envir = self$execution_state)
      } else {
        NULL
      }
    },
    
    # Serialize state for hot reload
    serialize_state = function(node_ids = NULL) {
      if (is.null(node_ids)) {
        node_ids <- ls(envir = self$values)
      }
      
      state_list <- list()
      for (node_id in node_ids) {
        value <- self$get_value(node_id)
        # Try to serialize value
        tryCatch({
          # For complex objects, we might need special handling
          state_list[[node_id]] <- value
        }, error = function(e) {
          warning("Could not serialize state for node: ", node_id)
        })
      }
      
      state_list
    },
    
    # Restore state from serialized data
    restore_state = function(state_list) {
      for (node_id in names(state_list)) {
        self$set_value(node_id, state_list[[node_id]])
      }
    },
    
    # Clear all state
    clear_all = function() {
      self$values <- new.env(parent = emptyenv())
      self$dirty_nodes <- new.env(parent = emptyenv())
      self$execution_state <- new.env(parent = emptyenv())
    }
  )
)

# Context for executor
executor_context <- new.env(parent = emptyenv())

#' Set the current executor
#'
#' @param executor Executor instance
set_executor <- function(executor) {
  assign("current_executor", executor, envir = executor_context)
}

#' Get the current executor
#'
#' @return Executor instance or NULL
get_executor <- function() {
  get("current_executor", envir = executor_context, inherits = FALSE)
}
