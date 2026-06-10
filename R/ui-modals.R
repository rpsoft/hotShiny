# Modal and Notification Functions
# Implements modals, notifications, and progress indicators

# ============================================================================
# Modal Dialogs
# ============================================================================

#' Create a modal dialog UI
#'
#' @param ... UI elements for modal body
#' @param title Modal title
#' @param footer Modal footer content (default: OK and Cancel buttons)
#' @param size Modal size: "s", "m", "l", "xl"
#' @param easyClose Close on click outside or Escape?
#' @param fade Use fade animation?
#' @return A modal dialog tag
#' @export
modalDialog <- function(..., title = NULL, footer = modalButton("Dismiss"),
                        size = c("m", "s", "l", "xl"), easyClose = FALSE, fade = TRUE) {
  size <- match.arg(size)
  
  # Size class
  size_class <- switch(size,
    "s" = "modal-sm",
    "m" = "",
    "l" = "modal-lg",
    "xl" = "modal-xl"
  )
  
  # Build header
  header <- NULL
  if (!is.null(title)) {
    header <- tag("div", attribs = list(class = "modal-header"),
      children = list(
        tag("h5", attribs = list(class = "modal-title"), children = list(title)),
        tag("button",
          attribs = list(
            type = "button",
            class = "btn-close",
            `data-bs-dismiss` = "modal",
            `aria-label` = "Close"
          ),
          children = list()
        )
      )
    )
  }
  
  # Build body
  body <- tag("div", attribs = list(class = "modal-body"), children = list(...))
  
  # Build footer
  footer_tag <- NULL
  if (!is.null(footer)) {
    footer_tag <- tag("div", attribs = list(class = "modal-footer"), children = list(footer))
  }
  
  # Dialog content
  dialog_class <- paste("modal-dialog", size_class)
  dialog_children <- list(header, body, footer_tag)
  dialog_children <- Filter(Negate(is.null), dialog_children)
  
  dialog <- tag("div", attribs = list(class = "modal-content"), children = dialog_children)
  
  # Modal wrapper
  modal_class <- if (fade) "modal fade" else "modal"
  modal_attribs <- list(
    class = modal_class,
    tabindex = "-1",
    `aria-hidden` = "true",
    `data-easy-close` = if (easyClose) "true" else "false"
  )
  
  if (!easyClose) {
    modal_attribs$`data-bs-backdrop` <- "static"
    modal_attribs$`data-bs-keyboard` <- "false"
  }
  
  modal <- tag("div", attribs = modal_attribs,
    children = list(
      tag("div", attribs = list(class = dialog_class), children = list(dialog))
    )
  )
  
  class(modal) <- c("shiny.modal", class(modal))
  modal
}

#' Create a modal button
#'
#' Creates a button that dismisses the modal.
#'
#' @param label Button label
#' @param icon Optional icon
#' @return A modal button tag
#' @export
modalButton <- function(label, icon = NULL) {
  children <- list()
  if (!is.null(icon)) {
    children <- c(children, list(icon, " "))
  }
  children <- c(children, list(label))
  
  tag("button",
    attribs = list(
      type = "button",
      class = "btn btn-secondary",
      `data-bs-dismiss` = "modal"
    ),
    children = children
  )
}

#' Show a modal dialog
#'
#' @param ui Modal UI (from modalDialog)
#' @param session Shiny session object
#' @return NULL (called for side effect)
#' @export
showModal <- function(ui, session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    warning("showModal called without a valid session")
    return(invisible(NULL))
  }
  
  # Convert UI to HTML
  html <- if (inherits(ui, "shiny.tag") || inherits(ui, "shiny.modal")) {
    tag_to_html(ui)
  } else {
    as.character(ui)
  }
  
  # Send message to client
  session$sendCustomMessage("shiny-modal", list(
    action = "show",
    html = html
  ))
  
  invisible(NULL)
}

#' Remove a modal dialog
#'
#' @param session Shiny session object
#' @return NULL (called for side effect)
#' @export
removeModal <- function(session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    return(invisible(NULL))
  }
  
  session$sendCustomMessage("shiny-modal", list(action = "remove"))
  invisible(NULL)
}

#' Create a modal dialog with a URL
#'
#' @param url URL to display
#' @param title Modal title
#' @param subtitle Optional subtitle
#' @return A modal dialog tag
#' @export
urlModal <- function(url, title = "Paste URL into browser:", subtitle = NULL) {
  body_children <- list()
  
  if (!is.null(subtitle)) {
    body_children <- c(body_children, list(
      tag("p", children = list(subtitle))
    ))
  }
  
  body_children <- c(body_children, list(
    tag("input",
      attribs = list(
        type = "text",
        class = "form-control",
        value = url,
        readonly = "readonly",
        style = "width: 100%;"
      ),
      children = list()
    )
  ))
  
  modalDialog(
    tagList(body_children),
    title = title,
    footer = modalButton("Close"),
    easyClose = TRUE
  )
}

# ============================================================================
# Notifications
# ============================================================================

#' Show a notification
#'
#' @param ui Notification content
#' @param action Optional action button/link
#' @param duration Duration in seconds (NULL for persistent)
#' @param closeButton Show close button?
#' @param id Unique ID (for removal)
#' @param type Type: "default", "message", "warning", "error"
#' @param session Shiny session
#' @return The notification ID
#' @export
showNotification <- function(ui, action = NULL, duration = 5, closeButton = TRUE,
                             id = NULL, type = c("default", "message", "warning", "error"),
                             session = getDefaultReactiveDomain()) {
  type <- match.arg(type)
  
  if (is.null(id)) {
    id <- paste0("notification_", sample.int(100000, 1))
  }
  
  # Type to Bootstrap alert class
  alert_class <- switch(type,
    "default" = "alert-primary",
    "message" = "alert-info",
    "warning" = "alert-warning",
    "error" = "alert-danger"
  )
  
  # Build notification HTML
  children <- list()
  
  # Content
  if (is.character(ui)) {
    children <- c(children, list(ui))
  } else {
    children <- c(children, list(tag_to_html(ui)))
  }
  
  # Action
  if (!is.null(action)) {
    children <- c(children, list(
      tag("div", attribs = list(class = "mt-2"), children = list(action))
    ))
  }
  
  # Close button
  close_btn <- NULL
  if (closeButton) {
    close_btn <- tag("button",
      attribs = list(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "alert",
        `aria-label` = "Close"
      ),
      children = list()
    )
  }
  
  notification <- tag("div",
    attribs = list(
      id = id,
      class = paste("alert", alert_class, "alert-dismissible fade show shiny-notification"),
      role = "alert",
      `data-duration` = if (!is.null(duration)) duration * 1000 else NULL
    ),
    children = c(children, list(close_btn))
  )
  
  # Send to client
  if (!is.null(session)) {
    html <- tag_to_html(notification)
    session$sendCustomMessage("shiny-notification", list(
      action = "show",
      id = id,
      html = html,
      duration = duration
    ))
  }
  
  invisible(id)
}

#' Remove a notification
#'
#' @param id Notification ID
#' @param session Shiny session
#' @return NULL (called for side effect)
#' @export
removeNotification <- function(id, session = getDefaultReactiveDomain()) {
  if (!is.null(session)) {
    session$sendCustomMessage("shiny-notification", list(
      action = "remove",
      id = id
    ))
  }
  invisible(NULL)
}

# ============================================================================
# Progress Indicators
# ============================================================================

#' Progress indicator (R6 class)
#'
#' @description
#' Object-oriented interface for progress indicators.
#'
#' @export
Progress <- R6::R6Class("Progress",
  public = list(
    #' @field session Shiny session
    session = NULL,
    #' @field id Progress ID
    id = NULL,
    #' @field min Minimum value
    min = 0,
    #' @field max Maximum value
    max = 1,
    
    #' @description Create a new Progress object
    #' @param session Shiny session
    #' @param min Minimum value
    #' @param max Maximum value
    initialize = function(session = getDefaultReactiveDomain(), min = 0, max = 1) {
      self$session <- session
      self$min <- min
      self$max <- max
      self$id <- paste0("progress_", sample.int(100000, 1))
      
      # Send initial message
      if (!is.null(self$session)) {
        self$session$sendCustomMessage("shiny-progress", list(
          action = "open",
          id = self$id
        ))
      }
    },
    
    #' @description Set progress value
    #' @param value Current value
    #' @param message Optional message
    #' @param detail Optional detail text
    set = function(value = NULL, message = NULL, detail = NULL) {
      if (!is.null(self$session)) {
        # Normalize value to 0-100
        pct <- NULL
        if (!is.null(value)) {
          pct <- (value - self$min) / (self$max - self$min) * 100
          pct <- max(0, min(100, pct))
        }
        
        self$session$sendCustomMessage("shiny-progress", list(
          action = "set",
          id = self$id,
          value = pct,
          message = message,
          detail = detail
        ))
      }
      invisible(self)
    },
    
    #' @description Increment progress
    #' @param amount Amount to increment
    #' @param message Optional message
    #' @param detail Optional detail text
    inc = function(amount = 0.1, message = NULL, detail = NULL) {
      if (!is.null(self$session)) {
        self$session$sendCustomMessage("shiny-progress", list(
          action = "inc",
          id = self$id,
          amount = amount * 100,
          message = message,
          detail = detail
        ))
      }
      invisible(self)
    },
    
    #' @description Close progress indicator
    close = function() {
      if (!is.null(self$session)) {
        self$session$sendCustomMessage("shiny-progress", list(
          action = "close",
          id = self$id
        ))
      }
      invisible(self)
    }
  )
)

#' Execute code with progress indicator
#'
#' @param expr Expression to execute
#' @param min Minimum value
#' @param max Maximum value
#' @param value Initial value
#' @param message Initial message
#' @param detail Initial detail
#' @param style Style: "notification" or "old"
#' @param session Shiny session
#' @param env Environment for evaluation
#' @param quoted Is expr quoted?
#' @return Result of expression
#' @export
withProgress <- function(expr, min = 0, max = 1, value = min + (max - min) * 0.1,
                         message = NULL, detail = NULL, style = "notification",
                         session = getDefaultReactiveDomain(), env = parent.frame(),
                         quoted = FALSE) {
  
  if (!quoted) {
    expr <- substitute(expr)
  }
  
  # Create progress object
  progress <- Progress$new(session = session, min = min, max = max)
  
  # Set initial state
  progress$set(value = value, message = message, detail = detail)
  
  # Ensure cleanup
  on.exit(progress$close(), add = TRUE)
  
  # Evaluate expression
  eval(expr, envir = env)
}

#' Set progress value
#'
#' @param value Progress value
#' @param message Optional message
#' @param detail Optional detail
#' @param session Shiny session
#' @return NULL (called for side effect)
#' @export
setProgress <- function(value = NULL, message = NULL, detail = NULL,
                        session = getDefaultReactiveDomain()) {
  if (!is.null(session)) {
    session$sendCustomMessage("shiny-progress", list(
      action = "set",
      value = if (!is.null(value)) value * 100 else NULL,
      message = message,
      detail = detail
    ))
  }
  invisible(NULL)
}

#' Increment progress value
#'
#' @param amount Amount to increment (0-1 scale)
#' @param message Optional message
#' @param detail Optional detail
#' @param session Shiny session
#' @return NULL (called for side effect)
#' @export
incProgress <- function(amount = 0.1, message = NULL, detail = NULL,
                        session = getDefaultReactiveDomain()) {
  if (!is.null(session)) {
    session$sendCustomMessage("shiny-progress", list(
      action = "inc",
      amount = amount * 100,
      message = message,
      detail = detail
    ))
  }
  invisible(NULL)
}

# ============================================================================
# Helper Functions
# ============================================================================

#' Get default reactive domain (session)
#'
#' @return Current session or NULL
#' @export
getDefaultReactiveDomain <- function() {
  tryCatch({
    # 1. The active session set during server execution / reactive evaluation.
    session <- get_current_session()
    if (!is.null(session)) {
      return(session)
    }
    # 2. Fall back to the executor's session if one has been attached.
    executor <- get_executor()
    if (!is.null(executor) && !is.null(executor$session)) {
      return(executor$session)
    }
    NULL
  }, error = function(e) NULL)
}
