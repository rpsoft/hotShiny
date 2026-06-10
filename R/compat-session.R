# Shiny Session Compatibility
# Provides a real `session` object (the third argument of server functions)
# implementing the most commonly used parts of Shiny's session API.
#
# In classic Shiny `session` is per-client. hotShiny currently runs a single
# shared reactive graph, so this is an app-scoped session that broadcasts to
# all connected clients. That is sufficient for the overwhelming majority of
# apps that only use `session` for sendCustomMessage / modules / userData, and
# it makes hotShiny's own modal / notification / dynamic-UI helpers work (they
# all call `session$sendCustomMessage()` and previously received `NULL`).

#' Shiny Session Proxy
#'
#' App-scoped session object passed as the third argument to server functions.
#' Implements the documented surface that real-world apps rely on.
ShinySession <- R6::R6Class("ShinySession",
  public = list(
    #' @field app The HotShinyApp instance (used to reach the WebSocket server)
    app = NULL,
    #' @field ns_prefix Namespace prefix (set for module sessions)
    ns_prefix = NULL,
    #' @field userData Environment for arbitrary user state (like Shiny)
    userData = NULL,
    #' @field clientData Environment of client-reported values
    clientData = NULL,
    #' @field input Input proxy (shared with the server function)
    input = NULL,
    #' @field output Output proxy (shared with the server function)
    output = NULL,
    #' @field request Stub HTTP request environment
    request = NULL,
    ended_callbacks = NULL,
    flush_callbacks = NULL,
    flushed_callbacks = NULL,
    custom_message_handlers = NULL,
    input_handlers = NULL,

    initialize = function(app = NULL, ns_prefix = NULL, input = NULL, output = NULL) {
      self$app <- app
      self$ns_prefix <- ns_prefix
      self$input <- input
      self$output <- output
      self$userData <- new.env(parent = emptyenv())
      self$clientData <- new.env(parent = emptyenv())
      self$request <- new.env(parent = emptyenv())
      self$ended_callbacks <- list()
      self$flush_callbacks <- list()
      self$flushed_callbacks <- list()
      self$custom_message_handlers <- new.env(parent = emptyenv())
      self$input_handlers <- new.env(parent = emptyenv())
    },

    # Namespace an id. For module sessions ns_prefix is set; for the root
    # session ns() is the identity, matching Shiny's behaviour.
    ns = function(id) {
      if (is.null(self$ns_prefix) || !nzchar(self$ns_prefix)) {
        return(id)
      }
      paste0(self$ns_prefix, "-", id)
    },

    # Send a custom message to the client(s). Wired to the WebSocket server so
    # that registered JS handlers (Shiny.addCustomMessageHandler) receive it.
    sendCustomMessage = function(type, message) {
      ws <- private$get_ws()
      if (is.null(ws)) {
        log_debug("[Session] sendCustomMessage: no WebSocket server yet, dropping '", type, "'\n", file = stderr())
        return(invisible(NULL))
      }
      ws$broadcast(type, message)
      invisible(NULL)
    },

    # Send a message targeting a specific input (used by update* functions).
    sendInputMessage = function(inputId, message) {
      self$sendCustomMessage("shiny-input-message", list(
        id = self$ns(inputId),
        message = message
      ))
    },

    # Programmatically set an input value (server-side), mirroring the effect
    # of the client reporting a new value.
    setInputValue = function(inputId, value) {
      executor <- tryCatch(get_executor(), error = function(e) NULL)
      if (!is.null(executor)) {
        executor$set_input(self$ns(inputId), value)
      }
      invisible(NULL)
    },

    # Register a server-side custom message handler (rare, but part of the API).
    registerDataObj = function(name, data, filter) {
      warning("session$registerDataObj() is not supported in hotShiny")
      invisible(NULL)
    },

    # Lifecycle callbacks --------------------------------------------------
    onSessionEnded = function(callback) {
      self$ended_callbacks <- c(self$ended_callbacks, list(callback))
      invisible(function() NULL)
    },
    onFlush = function(callback, once = TRUE) {
      self$flush_callbacks <- c(self$flush_callbacks, list(callback))
      invisible(function() NULL)
    },
    onFlushed = function(callback, once = TRUE) {
      self$flushed_callbacks <- c(self$flushed_callbacks, list(callback))
      invisible(function() NULL)
    },

    # Run lifecycle callbacks (called by the runtime).
    fire_flush = function() {
      for (cb in self$flush_callbacks) tryCatch(cb(), error = function(e) NULL)
    },
    fire_flushed = function() {
      for (cb in self$flushed_callbacks) tryCatch(cb(), error = function(e) NULL)
    },
    fire_ended = function() {
      for (cb in self$ended_callbacks) tryCatch(cb(), error = function(e) NULL)
    },

    # Ask the client to reload the page.
    reload = function() {
      self$sendCustomMessage("shiny-reload", list())
    },

    # Close the session (no-op for the shared session beyond firing callbacks).
    close = function() {
      self$fire_ended()
      invisible(NULL)
    },

    # Build a child session for a module namespace.
    makeScope = function(namespace) {
      child_prefix <- if (is.null(self$ns_prefix) || !nzchar(self$ns_prefix)) {
        namespace
      } else {
        paste0(self$ns_prefix, "-", namespace)
      }
      ShinySession$new(
        app = self$app,
        ns_prefix = child_prefix,
        input = self$input,
        output = self$output
      )
    }
  ),
  private = list(
    get_ws = function() {
      if (!is.null(self$app) && !is.null(self$app$ws_server)) {
        return(self$app$ws_server)
      }
      executor <- tryCatch(get_executor(), error = function(e) NULL)
      if (!is.null(executor) && !is.null(executor$app) && !is.null(executor$app$ws_server)) {
        return(executor$app$ws_server)
      }
      NULL
    }
  )
)

# Context tracking for the "current" reactive domain (session). This lets
# getDefaultReactiveDomain() return the active session, matching Shiny.
session_context <- new.env(parent = emptyenv())

#' Set the current reactive domain (session)
#'
#' @param session ShinySession instance or NULL
#' @keywords internal
set_current_session <- function(session) {
  assign("current_session", session, envir = session_context)
}

#' Get the current reactive domain (session)
#'
#' @return ShinySession or NULL
#' @keywords internal
get_current_session <- function() {
  if (exists("current_session", envir = session_context, inherits = FALSE)) {
    get("current_session", envir = session_context, inherits = FALSE)
  } else {
    NULL
  }
}
