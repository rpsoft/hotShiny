# WebSocket Server
# Handles WebSocket communication with clients

#' WebSocket Message Types
WS_MESSAGE_TYPES <- list(
  GRAPH_UPDATE = "graph_update",
  VALUE_UPDATE = "value_update",
  DOM_PATCH = "dom_patch",
  USER_INPUT = "user_input",
  HOT_RELOAD = "hot_reload",
  ERROR = "error",
  PING = "ping",
  PONG = "pong"
)

#' WebSocket Server
#'
#' Manages WebSocket connections and communication
WebSocketServer <- R6::R6Class("WebSocketServer",
  public = list(
    app = NULL,
    connections = NULL,
    message_handlers = NULL,
    
    initialize = function(app) {
      self$app <- app
      self$connections <- list()
      self$message_handlers <- new.env(parent = emptyenv())
      
      # Register default handlers
      self$register_handler(WS_MESSAGE_TYPES$USER_INPUT, function(msg, ws) {
        self$handle_user_input(msg, ws)
      })
      
      self$register_handler(WS_MESSAGE_TYPES$PING, function(msg, ws) {
        self$send_message(ws, WS_MESSAGE_TYPES$PONG, list())
      })
      
      self$register_handler(WS_MESSAGE_TYPES$PONG, function(msg, ws) {
        # Pong received, connection is alive
        # No action needed
      })
    },
    
    # Register a message handler
    register_handler = function(message_type, handler) {
      self$message_handlers[[message_type]] <- handler
    },
    
    # Handle new WebSocket connection
    on_open = function(ws) {
      connection_id <- paste0("conn_", length(self$connections) + 1L)
      self$connections[[connection_id]] <- ws
      
      # Send initial graph
      self$send_graph_update(ws)
      
      # Send initial values for all output nodes
      # First trigger execution to compute all values, then send updates
      executor <- self$app$get_executor()
      if (!is.null(executor)) {
        # Execute all nodes to compute initial values
        log_debug("[WebSocket] on_open: Executing all nodes for initial values\n", file = stderr())
        executor$execute()
        log_debug("[WebSocket] on_open: Execution complete, sending output updates\n", file = stderr())
        # Use send_output_updates which properly finds all render nodes
        executor$send_output_updates()
      }
      
      connection_id
    },
    
    # Handle WebSocket message
    on_message = function(ws, is_binary, message) {
      tryCatch({
        # Parse message
        if (is_binary) {
          # Binary message (would need deserialization)
          # For now, assume JSON
          msg <- jsonlite::fromJSON(rawToChar(message), simplifyVector = FALSE)
        } else {
          msg <- jsonlite::fromJSON(message, simplifyVector = FALSE)
        }
        
        # Get message type
        msg_type <- msg$type
        if (is.null(msg_type)) {
          warning("Message missing type field")
          return(invisible(NULL))
        }
        
        # Find handler
        handler <- self$message_handlers[[msg_type]]
        if (!is.null(handler)) {
          # Execute handler with error handling
          tryCatch({
            handler(msg, ws)
          }, error = function(e) {
            # Catch errors in handlers - don't send to client
            # These are internal errors that should be handled gracefully
            error_msg <- conditionMessage(e)
            
            # Only log non-input-related errors
            if (!grepl("object.*input", error_msg, ignore.case = TRUE)) {
              warning("Error in message handler for type '", msg_type, "': ", error_msg)
            }
            # Don't send error to client - it's an internal error
          })
        } else {
          warning("No handler for message type: ", msg_type)
        }
        
      }, error = function(e) {
        # Only send errors to client for message parsing errors
        # Internal execution errors should not be sent
        error_msg <- conditionMessage(e)
        if (grepl("JSON|parse|message", error_msg, ignore.case = TRUE)) {
          # Message parsing error - can send to client
          self$send_error(ws, "Invalid message format")
        } else {
          # Internal error - don't send to client
          warning("Internal error handling message: ", error_msg)
        }
      })
    },
    
    # Handle WebSocket close
    on_close = function(ws) {
      # Remove connection
      for (i in seq_along(self$connections)) {
        if (identical(self$connections[[i]], ws)) {
          self$connections <- self$connections[-i]
          break
        }
      }
    },
    
    # Send message to a WebSocket connection
    send_message = function(ws, type, data) {
      message <- list(
        type = type,
        data = data,
        timestamp = as.numeric(Sys.time())
      )
      
      json_msg <- jsonlite::toJSON(message, auto_unbox = TRUE)
      
      # Send via httpuv
      if (!is.null(ws) && is.function(ws$send)) {
        ws$send(json_msg)
      }
      
      invisible(NULL)
    },
    
    # Send graph update
    send_graph_update = function(ws) {
      graph <- self$app$get_graph()
      
      # Get serialize_graph function
      ns <- asNamespace("hotShiny")
      base_path <- getwd()
      if (!file.exists(file.path(base_path, "R"))) {
        base_path <- system.file(package = "hotShiny")
      }
      load_env <- new.env(parent = ns)
      serializer_file <- file.path(base_path, "R/ir/serializer.R")
      if (file.exists(serializer_file)) {
        sys.source(serializer_file, envir = load_env)
      }
      
      serialize_fn <- if (exists("serialize_graph", envir = load_env)) {
        get("serialize_graph", envir = load_env)
      } else if (exists("serialize_graph", envir = ns)) {
        get("serialize_graph", envir = ns)
      } else {
        # Fallback: simple serialization
        function(g) jsonlite::toJSON(list(nodes = length(g$nodes)), auto_unbox = TRUE)
      }
      
      graph_json <- serialize_fn(graph)
      graph_data <- jsonlite::fromJSON(graph_json, simplifyVector = FALSE)
      
      self$send_message(ws, WS_MESSAGE_TYPES$GRAPH_UPDATE, graph_data)
    },
    
    # Send value update
    send_value_update = function(node_id, value, output_name = NULL) {
      update_data <- list(
        node_id = node_id,
        value = value
      )
      
      # Include output_name if provided
      if (!is.null(output_name)) {
        update_data$output_name <- output_name
      }
      
      # Send to all connections
      for (conn_id in names(self$connections)) {
        conn <- self$connections[[conn_id]]
        if (!is.null(conn)) {
          self$send_message(conn, WS_MESSAGE_TYPES$VALUE_UPDATE, update_data)
        }
      }
    },
    
    # Send DOM patch
    send_dom_patch = function(patch) {
      # Send to all connections
      for (conn in self$connections) {
        self$send_message(conn, WS_MESSAGE_TYPES$DOM_PATCH, patch)
      }
    },
    
    # Send hot reload notification
    send_hot_reload = function(diff_summary) {
      reload_data <- list(
        summary = diff_summary,
        timestamp = as.numeric(Sys.time())
      )
      
      # Send to all connections
      for (conn in self$connections) {
        self$send_message(conn, WS_MESSAGE_TYPES$HOT_RELOAD, reload_data)
      }
    },
    
    # Send UI replacement (for hot reload UI updates)
    send_ui_replace = function(html) {
      ui_data <- list(
        html = html,
        selector = "#app"
      )
      
      # Send to all connections as a custom message type
      # The client will register a handler for 'shiny-replace-ui'
      for (conn in self$connections) {
        self$send_message(conn, "shiny-replace-ui", ui_data)
      }
    },
    
    # Send error
    send_error = function(ws, error_message) {
      error_data <- list(
        message = error_message,
        timestamp = as.numeric(Sys.time())
      )
      
      self$send_message(ws, WS_MESSAGE_TYPES$ERROR, error_data)
    },
    
    # Handle user input
    handle_user_input = function(msg, ws) {
      data <- msg$data
      if (is.null(data$input_name) || is.null(data$value)) {
        warning("[WebSocket] Invalid user input message")
        return(invisible(NULL))
      }
      
      log_debug("[WebSocket] Received user_input: ", data$input_name, " = ", data$value, "\n", file=stderr())
      
      # Set input value in executor with error handling
      tryCatch({
        executor <- self$app$get_executor()
        if (is.null(executor)) {
          log_debug("[WebSocket] ERROR: Executor is NULL! App:", if(is.null(self$app)) "NULL" else "exists", "\n", file=stderr())
          warning("[WebSocket] Executor is NULL!")
          return(invisible(NULL))
        }
        log_debug("[WebSocket] Executor found, calling set_input for", data$input_name, "=", data$value, "\n", file=stderr())
        executor$set_input(data$input_name, data$value)
        log_debug("[WebSocket] executor$set_input completed successfully\n", file=stderr())
      }, error = function(e) {
        log_debug("[WebSocket] ERROR in handle_user_input:", conditionMessage(e), "\n", file=stderr())
        log_debug("[WebSocket] Error traceback:", paste(capture.output(traceback()), collapse="\n"), "\n", file=stderr())
        # Don't send errors to client - they are internal errors
        # The executor's set_input already handles errors internally
        error_msg <- conditionMessage(e)
        warning("[WebSocket] Error handling user input: ", error_msg)
      })
    },
    
    # Broadcast to all connections
    broadcast = function(type, data) {
      for (conn in self$connections) {
        self$send_message(conn, type, data)
      }
    }
  )
)

#' Create WebSocket server for app
#'
#' @param app HotShinyApp instance
#' @return WebSocketServer instance
create_websocket_server <- function(app) {
  WebSocketServer$new(app)
}
