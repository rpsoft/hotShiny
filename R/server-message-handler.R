# Message Handler
# Handles WebSocket messages

#' Message Handler
#'
#' Processes and routes WebSocket messages
MessageHandler <- R6::R6Class("MessageHandler",
  public = list(
    app = NULL,
    handlers = NULL,
    
    initialize = function(app) {
      self$app <- app
      self$handlers <- new.env(parent = emptyenv())
    },
    
    # Register handler for message type
    register = function(message_type, handler) {
      self$handlers[[message_type]] <- handler
    },
    
    # Handle message
    handle = function(message, ws) {
      msg_type <- message$type
      
      handler <- self$handlers[[msg_type]]
      if (!is.null(handler)) {
        handler(message, ws)
      } else {
        warning("No handler for message type: ", msg_type)
      }
    }
  )
)

#' Create message handler
#'
#' @param app HotShinyApp instance
#' @return MessageHandler instance
create_message_handler <- function(app) {
  handler <- MessageHandler$new(app)
  
  # Register default handlers
  handler$register(WS_MESSAGE_TYPES$USER_INPUT, function(msg, ws) {
    # Handle user input
    data <- msg$data
    executor <- app$get_executor()
    if (!is.null(executor) && !is.null(data$input_name)) {
      executor$set_input(data$input_name, data$value)
    }
  })
  
  handler
}
