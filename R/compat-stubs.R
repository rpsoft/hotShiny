# Graceful-Failure Stubs for Unsupported Shiny API
# Functions in this file exist so that swapping library(shiny) for
# library(hotShiny) fails *loudly and informatively* rather than with a cryptic
# "could not find function" error. Each throws a catchable condition of class
# "hotshiny_unsupported" with guidance, instead of silently doing the wrong
# thing. The shiny2hotshiny translator (check_app()) uses the same inventory to
# flag these calls ahead of time.

#' Signal that a Shiny feature is not supported by hotShiny
#'
#' @param feature Human-readable feature name
#' @param hint Optional guidance / alternative
#' @keywords internal
.unsupported <- function(feature, hint = NULL) {
  msg <- paste0(
    feature, " is not supported in hotShiny ",
    as.character(utils::packageVersion("hotShiny")), ".")
  if (!is.null(hint)) {
    msg <- paste0(msg, "\n  ", hint)
  }
  msg <- paste0(
    msg,
    "\n  Run shiny2hotshiny_check(\"path/to/app\") to scan your app for ",
    "compatibility issues, or see COMPATIBILITY.md.")
  stop(structure(
    class = c("hotshiny_unsupported", "error", "condition"),
    list(message = msg, call = sys.call(-1))
  ))
}

# --- Bookmarking ------------------------------------------------------------

#' @rdname hotshiny-unsupported
#' @export
enableBookmarking <- function(store = c("url", "server", "disable")) {
  .unsupported("Bookmarking (enableBookmarking)",
               "hotShiny preserves state via hot reload, not URL bookmarking.")
}
#' @rdname hotshiny-unsupported
#' @export
setBookmarkExclude <- function(names = character(0), session = NULL) {
  .unsupported("Bookmarking (setBookmarkExclude)")
}
#' @rdname hotshiny-unsupported
#' @export
bookmarkButton <- function(...) .unsupported("Bookmarking (bookmarkButton)")
#' @rdname hotshiny-unsupported
#' @export
onBookmark <- function(fun, session = NULL) .unsupported("Bookmarking (onBookmark)")
#' @rdname hotshiny-unsupported
#' @export
onBookmarked <- function(fun, session = NULL) .unsupported("Bookmarking (onBookmarked)")
#' @rdname hotshiny-unsupported
#' @export
onRestore <- function(fun, session = NULL) .unsupported("Bookmarking (onRestore)")
#' @rdname hotshiny-unsupported
#' @export
onRestored <- function(fun, session = NULL) .unsupported("Bookmarking (onRestored)")

# --- Caching / async --------------------------------------------------------

#' @rdname hotshiny-unsupported
#' @export
bindCache <- function(x, ...) {
  .unsupported("bindCache()",
               "Result caching is not implemented; the reactive runs every time.")
}
#' @rdname hotshiny-unsupported
#' @export
ExtendedTask <- function(...) {
  .unsupported("ExtendedTask (async tasks)",
               "Long-running async tasks via promises are not yet supported.")
}

# --- Reactive polling / file reading ---------------------------------------

#' @rdname hotshiny-unsupported
#' @export
reactivePoll <- function(intervalMillis, session, checkFunc, valueFunc) {
  .unsupported("reactivePoll()",
               "Use invalidateLater() inside a reactive as a partial alternative.")
}
#' @rdname hotshiny-unsupported
#' @export
reactiveFileReader <- function(intervalMillis, session, filePath, readFunc, ...) {
  .unsupported("reactiveFileReader()",
               "Use invalidateLater() plus a manual read as a partial alternative.")
}

# --- Testing / snapshot helpers --------------------------------------------

#' @rdname hotshiny-unsupported
#' @export
exportTestValues <- function(...) .unsupported("exportTestValues() (shinytest)")
#' @rdname hotshiny-unsupported
#' @export
snapshotExclude <- function(x) .unsupported("snapshotExclude()")
#' @rdname hotshiny-unsupported
#' @export
markRenderFunction <- function(uiFunc, renderFunc, ...) {
  .unsupported("markRenderFunction()",
               "Define render functions with createRenderFunction() instead.")
}

# --- Launchers --------------------------------------------------------------

#' @rdname hotshiny-unsupported
#' @export
runExample <- function(...) {
  .unsupported("runExample()", "Run a hotShiny app file with runApp(\"app.R\").")
}
#' @rdname hotshiny-unsupported
#' @export
runGist <- function(...) .unsupported("runGist()")
#' @rdname hotshiny-unsupported
#' @export
runGitHub <- function(...) .unsupported("runGitHub()")
#' @rdname hotshiny-unsupported
#' @export
runUrl <- function(...) .unsupported("runUrl()")

# ---------------------------------------------------------------------------
# Lightweight implementations of small utilities that ARE easy to support.
# ---------------------------------------------------------------------------

#' Set hotShiny/Shiny options
#'
#' @param ... Named options to set
#' @export
shinyOptions <- function(...) {
  opts <- list(...)
  if (length(opts) > 0) {
    names(opts) <- paste0("shiny.", names(opts))
    do.call(options, opts)
  }
  invisible(opts)
}

#' Get a hotShiny/Shiny option
#'
#' @param name Option name (without the "shiny." prefix)
#' @param default Default value if unset
#' @export
getShinyOption <- function(name, default = NULL) {
  getOption(paste0("shiny.", name), default)
}

#' Parse a URL query string into a named list
#'
#' @param str Query string (with or without leading "?")
#' @param nested Unused; for signature compatibility
#' @return Named list of decoded parameters
#' @export
parseQueryString <- function(str, nested = FALSE) {
  if (is.null(str) || !nzchar(str)) return(list())
  str <- sub("^[?]", "", str)
  pairs <- strsplit(str, "&", fixed = TRUE)[[1]]
  pairs <- pairs[nzchar(pairs)]
  out <- list()
  for (p in pairs) {
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    key <- utils::URLdecode(gsub("+", " ", kv[1], fixed = TRUE))
    val <- if (length(kv) > 1) utils::URLdecode(gsub("+", " ", kv[2], fixed = TRUE)) else ""
    out[[key]] <- val
  }
  out
}

#' Register a callback to run when the app stops
#'
#' @param fun Zero-argument callback
#' @param session Reactive domain (session) to attach to
#' @export
onStop <- function(fun, session = getDefaultReactiveDomain()) {
  if (!is.null(session) && is.function(session$onSessionEnded)) {
    return(session$onSessionEnded(fun))
  }
  invisible(function() NULL)
}
