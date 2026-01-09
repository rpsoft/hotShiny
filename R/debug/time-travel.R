# Time-Travel Debugging
# Records graph state history and enables replay

#' Time-Travel Debugger
#'
#' Records state history and enables replay
TimeTravelDebugger <- R6::R6Class("TimeTravelDebugger",
  public = list(
    history = NULL,
    current_step = NULL,
    max_history = NULL,
    enabled = NULL,
    
    initialize = function(max_history = 1000) {
      self$history <- list()
      self$current_step <- 0L
      self$max_history <- max_history
      self$enabled <- FALSE
    },
    
    # Record a state snapshot
    record_snapshot = function(graph, state_manager, metadata = list()) {
      if (!self$enabled) {
        return(invisible(NULL))
      }
      
      # Serialize state
      state_snapshot <- state_manager$serialize_state()
      
      snapshot <- list(
        step = self$current_step + 1L,
        timestamp = Sys.time(),
        graph = graph$to_list(),
        state = state_snapshot,
        metadata = metadata
      )
      
      # Add to history
      self$history <- c(self$history, list(snapshot))
      self$current_step <- self$current_step + 1L
      
      # Trim history if too long
      if (length(self$history) > self$max_history) {
        self$history <- self$history[-1]
        self$current_step <- self$current_step - 1L
      }
      
      invisible(NULL)
    },
    
    # Get snapshot at step
    get_snapshot = function(step) {
      if (step < 1 || step > length(self$history)) {
        return(NULL)
      }
      
      self$history[[step]]
    },
    
    # Get current snapshot
    get_current = function() {
      if (self$current_step == 0 || length(self$history) == 0) {
        return(NULL)
      }
      
      self$history[[self$current_step]]
    },
    
    # Go to a specific step
    go_to_step = function(step, app) {
      snapshot <- self$get_snapshot(step)
      if (is.null(snapshot)) {
        warning("Step ", step, " not found in history")
        return(FALSE)
      }
      
      # Restore graph and state
      # This would require deserializing the graph
      # For now, just update state
      if (is.function(app$get_state_manager)) {
        state_manager <- app$get_state_manager()
        if (!is.null(state_manager)) {
          state_manager$restore_state(snapshot$state)
        }
      }
      
      self$current_step <- step
      TRUE
    },
    
    # Step forward
    step_forward = function(app) {
      if (self$current_step < length(self$history)) {
        self$go_to_step(self$current_step + 1L, app)
      }
    },
    
    # Step backward
    step_backward = function(app) {
      if (self$current_step > 1) {
        self$go_to_step(self$current_step - 1L, app)
      }
    },
    
    # Replay from beginning
    replay = function(app, callback = NULL) {
      for (i in seq_along(self$history)) {
        self$go_to_step(i, app)
        
        if (!is.null(callback)) {
          callback(i, self$history[[i]])
        }
        
        # Small delay for visualization
        Sys.sleep(0.1)
      }
    },
    
    # Clear history
    clear = function() {
      self$history <- list()
      self$current_step <- 0L
    },
    
    # Enable debugging
    enable = function() {
      self$enabled <- TRUE
    },
    
    # Disable debugging
    disable = function() {
      self$enabled <- FALSE
    },
    
    # Get history summary
    get_summary = function() {
      list(
        enabled = self$enabled,
        total_steps = length(self$history),
        current_step = self$current_step,
        max_history = self$max_history
      )
    }
  )
)

#' Enable time-travel debugging
#'
#' @param app HotShinyApp instance
#' @param max_history Maximum history size
#' @return App with debugging enabled
enable_time_travel <- function(app, max_history = 1000) {
  debugger <- attr(app, "debugger")
  if (is.null(debugger)) {
    attr(app, "debugger") <- TimeTravelDebugger$new(max_history = max_history)
    debugger <- attr(app, "debugger")
  }
  debugger$enable()
  attr(app, "debugger") <- debugger
  app
}

#' Disable time-travel debugging
#'
#' @param app HotShinyApp instance
#' @return App with debugging disabled
disable_time_travel <- function(app) {
  debugger <- attr(app, "debugger")
  if (!is.null(debugger)) {
    debugger$disable()
    attr(app, "debugger") <- debugger
  }
  app
}

#' Graph Visualizer
#'
#' Visualizes the reactive graph structure
GraphVisualizer <- R6::R6Class("GraphVisualizer",
  public = list(
    graph = NULL,
    
    initialize = function(graph) {
      self$graph <- graph
    },
    
    # Generate DOT format for graphviz
    to_dot = function() {
      nodes <- self$graph$get_all_nodes()
      edges <- self$graph$edges
      
      dot_lines <- c("digraph ReactiveGraph {")
      
      # Add nodes
      for (node in nodes) {
        label <- paste0(node$id, "\\n", node$type)
        dot_lines <- c(dot_lines, paste0('  "', node$id, '" [label="', label, '"];'))
      }
      
      # Add edges
      for (edge in edges) {
        dot_lines <- c(dot_lines, paste0('  "', edge$from, '" -> "', edge$to, '";'))
      }
      
      dot_lines <- c(dot_lines, "}")
      paste(dot_lines, collapse = "\n")
    },
    
    # Generate JSON for web visualization
    to_json = function() {
      nodes <- self$graph$get_all_nodes()
      
      node_list <- lapply(nodes, function(n) {
        list(
          id = n$id,
          type = n$type,
          deps = n$deps,
          label = paste(n$id, n$type, sep = " - ")
        )
      })
      
      list(
        nodes = node_list,
        edges = self$graph$edges
      )
    }
  )
)

#' Visualize graph
#'
#' @param graph ReactiveGraph
#' @param format Output format ("dot" or "json")
#' @return Graph visualization
visualize_graph <- function(graph, format = "dot") {
  visualizer <- GraphVisualizer$new(graph)
  
  switch(format,
    "dot" = visualizer$to_dot(),
    "json" = jsonlite::toJSON(visualizer$to_json(), pretty = TRUE),
    stop("Unknown format: ", format)
  )
}
