# Application-level Shiny Compatibility
# Drop-in aliases and helpers so that idiomatic Shiny app code
# (shinyApp(), shinyServer(), stopApp(), addResourcePath(), ...) works under
# hotShiny without modification.

#' Create a Shiny-style application object
#'
#' Compatibility alias for [app()]. Accepts the same arguments as Shiny's
#' `shinyApp()` so existing apps that end in `shinyApp(ui, server)` work.
#'
#' @param ui UI object or function
#' @param server Server function
#' @param onStart,options,uiPattern,enableBookmarking,... Accepted for
#'   signature compatibility (mostly ignored; onStart is run if supplied)
#' @return A HotShinyApp object
#' @export
shinyApp <- function(ui, server, onStart = NULL, options = list(),
                     uiPattern = "/", enableBookmarking = NULL, ...) {
  if (is.function(onStart)) {
    tryCatch(onStart(), error = function(e) {
      warning("onStart callback failed: ", conditionMessage(e))
    })
  }
  app(ui, server, ...)
}

#' Create a Shiny app from a directory
#'
#' Compatibility alias for running an app directory or app file.
#'
#' @param appDir Path to an app directory (containing app.R or ui.R/server.R)
#'   or to a single app file
#' @param options List of options (port/host honoured)
#' @param ... Passed through to [runApp()]
#' @export
shinyAppDir <- function(appDir, options = list(), ...) {
  runApp(appDir, ...)
}

#' Legacy server constructor
#'
#' @param func A server function `function(input, output, session)`
#' @return The function, marked as a Shiny server
#' @export
shinyServer <- function(func) {
  attr(func, "shiny.server") <- TRUE
  func
}

#' Legacy UI constructor
#'
#' @param ui A UI object
#' @return The UI object unchanged
#' @export
shinyUI <- function(ui) {
  ui
}

# Registry of a stop callback so stopApp() can halt the running event loop.
.app_stop_context <- new.env(parent = emptyenv())

#' Register the currently running app so stopApp() can stop it
#' @keywords internal
set_running_app <- function(app) {
  assign("running_app", app, envir = .app_stop_context)
}

#' Stop the currently running application
#'
#' @param returnValue Value to return from the (blocking) runApp call
#' @export
stopApp <- function(returnValue = invisible()) {
  app <- if (exists("running_app", envir = .app_stop_context, inherits = FALSE)) {
    get("running_app", envir = .app_stop_context, inherits = FALSE)
  } else {
    NULL
  }
  if (!is.null(app)) {
    app$running <- FALSE
    if (!is.null(app$ws_server)) {
      tryCatch(app$ws_server <- app$ws_server, error = function(e) NULL)
    }
  }
  assign("stop_return_value", returnValue, envir = .app_stop_context)
  invisible(returnValue)
}

# ---------------------------------------------------------------------------
# Static resource paths (addResourcePath / removeResourcePath)
# ---------------------------------------------------------------------------

.resource_paths <- new.env(parent = emptyenv())

#' Add a directory of static resources served under a URL prefix
#'
#' Files become available at `/<prefix>/<file>`. Used by htmlwidgets and other
#' packages that ship JavaScript/CSS assets.
#'
#' @param prefix URL prefix (no slashes)
#' @param directoryPath Local directory to serve
#' @export
addResourcePath <- function(prefix, directoryPath) {
  prefix <- sub("^/", "", prefix)
  assign(prefix, normalizePath(directoryPath, mustWork = FALSE), envir = .resource_paths)
  invisible(NULL)
}

#' Remove a previously registered resource path
#' @param prefix URL prefix to remove
#' @export
removeResourcePath <- function(prefix) {
  prefix <- sub("^/", "", prefix)
  if (exists(prefix, envir = .resource_paths, inherits = FALSE)) {
    rm(list = prefix, envir = .resource_paths)
  }
  invisible(NULL)
}

#' List registered resource paths
#' @keywords internal
get_resource_paths <- function() {
  paths <- list()
  for (p in ls(envir = .resource_paths)) {
    paths[[p]] <- get(p, envir = .resource_paths)
  }
  paths
}

#' Resolve a request path against the registered resource paths
#'
#' @param path Request path (e.g. "/mylib/foo.js")
#' @return Absolute file path if a registered prefix matches and the file
#'   exists, otherwise NULL
#' @keywords internal
resolve_resource_path <- function(path) {
  path <- sub("^/", "", path)
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(NULL)
  prefix <- parts[1]
  if (!exists(prefix, envir = .resource_paths, inherits = FALSE)) return(NULL)
  base <- get(prefix, envir = .resource_paths, inherits = FALSE)
  rel <- paste(parts[-1], collapse = "/")
  # Prevent path traversal.
  full <- normalizePath(file.path(base, rel), mustWork = FALSE)
  if (!startsWith(full, base)) return(NULL)
  if (file.exists(full) && !dir.exists(full)) return(full)
  NULL
}

# ---------------------------------------------------------------------------
# Input handlers (registerInputHandler / removeInputHandler)
# ---------------------------------------------------------------------------

.input_handlers <- new.env(parent = emptyenv())

#' Register a custom input type handler
#'
#' Custom input handlers coerce raw client values of a given `type` into R
#' objects (used by date inputs, htmlwidgets, etc.).
#'
#' @param type Input type string (e.g. "shiny.date")
#' @param fn Function `function(value, session, name)` returning the coerced value
#' @param force Overwrite an existing handler
#' @export
registerInputHandler <- function(type, fn, force = FALSE) {
  if (exists(type, envir = .input_handlers, inherits = FALSE) && !force) {
    stop("There is already an input handler for type: ", type)
  }
  assign(type, fn, envir = .input_handlers)
  invisible(NULL)
}

#' Remove a custom input type handler
#' @param type Input type string
#' @export
removeInputHandler <- function(type) {
  if (exists(type, envir = .input_handlers, inherits = FALSE)) {
    rm(list = type, envir = .input_handlers)
  }
  invisible(NULL)
}

#' Apply a registered input handler to a raw value
#' @keywords internal
apply_input_handler <- function(type, value, session = NULL, name = NULL) {
  if (!is.null(type) && exists(type, envir = .input_handlers, inherits = FALSE)) {
    fn <- get(type, envir = .input_handlers)
    return(tryCatch(fn(value, session, name), error = function(e) value))
  }
  value
}
