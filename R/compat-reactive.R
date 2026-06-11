# Reactive Compatibility Extras
# Implements the commonly used reactive primitives that Shiny apps rely on but
# that were missing from hotShiny: isolate(), req(), validate()/need(),
# reactiveVal(), eventReactive(), reactiveValuesToList(), is.reactive(),
# invalidateLater()/reactiveTimer(), debounce()/throttle(), bindEvent().
#
# Note on the execution model: hotShiny reconstructs reactive expressions from a
# serialized AST and evaluates them in a synthetic environment. Some Shiny
# patterns (writing to reactiveVal/reactiveValues from inside an observer) cannot
# be expressed faithfully under that model; those are detected by the
# shiny2hotshiny translator (see check_app()). See COMPATIBILITY.md.

# ---------------------------------------------------------------------------
# Silent-error condition system (req / validate / need)
# ---------------------------------------------------------------------------

#' Construct the silent error used to halt reactive evaluation
#' @keywords internal
shiny_silent_error <- function(class = NULL, message = "") {
  structure(
    class = c(class, "shiny.silent.error", "error", "condition"),
    list(message = message, call = NULL)
  )
}

#' Stop quietly, halting the current reactive computation without an error toast
#'
#' @param message Optional message
#' @export
reqStop <- function(message = "") {
  stop(shiny_silent_error("validation", message))
}

#' Require that values are available (truthy), else stop silently
#'
#' Mirrors Shiny's `req()`: if any argument is "falsy" (NULL, FALSE, "", an
#' empty vector, or a try-error) evaluation of the enclosing reactive stops
#' silently and the output is left blank.
#'
#' @param ... Values to check
#' @param cancelOutput If TRUE, keep the previous output instead of clearing it
#' @return The last argument, invisibly, if all are truthy
#' @export
req <- function(..., cancelOutput = FALSE) {
  dots <- list(...)
  for (val in dots) {
    if (!isTruthy(val)) {
      if (isTRUE(cancelOutput)) {
        stop(shiny_silent_error("shiny.output.cancel", "cancelOutput"))
      }
      stop(shiny_silent_error("validation", ""))
    }
  }
  if (length(dots) == 0) {
    return(invisible(NULL))
  }
  invisible(dots[[length(dots)]])
}

#' Test whether a value is "truthy" in the Shiny sense
#'
#' @param x Value to test
#' @return TRUE or FALSE
#' @export
isTruthy <- function(x) {
  if (inherits(x, "try-error")) return(FALSE)
  if (is.null(x)) return(FALSE)
  if (length(x) == 0) return(FALSE)
  if (is.function(x)) return(TRUE)
  if (inherits(x, "shiny.silent.error")) return(FALSE)
  if (is.logical(x) || is.character(x)) {
    if (any(is.na(x))) return(FALSE)
    if (is.logical(x) && !any(x)) return(FALSE)
    if (is.character(x) && !any(nzchar(x))) return(FALSE)
  }
  if (is.atomic(x) && all(is.na(x))) return(FALSE)
  TRUE
}

#' Validate reactive inputs, producing user-facing messages
#'
#' @param ... Results of `need()` calls (strings or NULL)
#' @param errorClass Additional condition class
#' @export
validate <- function(..., errorClass = character(0)) {
  results <- c(...)
  results <- results[!vapply(results, is.null, logical(1))]
  # Keep only non-empty character messages
  msgs <- Filter(function(m) is.character(m) && nzchar(m), results)
  if (length(msgs) > 0) {
    stop(shiny_silent_error(c(errorClass, "validation"), paste(unlist(msgs), collapse = "\n")))
  }
  invisible()
}

#' Express a validation requirement
#'
#' @param expr Condition that must be truthy
#' @param message Message to show when the condition fails
#' @param label Optional label
#' @return NULL if `expr` is truthy, otherwise `message`
#' @export
need <- function(expr, message = paste(label, "must be provided"), label) {
  force(message)
  if (!isTruthy(expr)) {
    return(message)
  }
  invisible(NULL)
}

#' Wrap an error so its message is shown to the user
#' @param error An error/condition or message string
#' @export
safeError <- function(error) {
  msg <- if (inherits(error, "condition")) conditionMessage(error) else as.character(error)
  structure(
    class = c("shiny.custom.error", "shiny.silent.error", "error", "condition"),
    list(message = msg, call = NULL)
  )
}

# ---------------------------------------------------------------------------
# isolate()
# ---------------------------------------------------------------------------

#' Evaluate an expression without taking a reactive dependency
#'
#' In hotShiny dependency tracking is static, so `isolate()` is handled at two
#' levels: extract_dependencies() does not descend into `isolate(...)` calls, and
#' at runtime this simply returns the already-evaluated value.
#'
#' @param expr Expression to evaluate in isolation
#' @export
isolate <- function(expr) {
  expr
}

# ---------------------------------------------------------------------------
# is.reactive / reactiveValuesToList
# ---------------------------------------------------------------------------

#' Is an object a reactive expression?
#' @param x Object to test
#' @export
is.reactive <- function(x) {
  inherits(x, "ReactiveProxy") || inherits(x, "ReactiveValProxy") ||
    inherits(x, "reactive") || inherits(x, "reactiveExpr")
}

#' Convert a reactiveValues object to a plain list
#' @param x A reactiveValues object
#' @param all.names Include names beginning with a dot
#' @export
reactiveValuesToList <- function(x, all.names = FALSE) {
  if (inherits(x, "ReactiveValues")) {
    vals <- as.list(x$values, all.names = all.names)
    return(vals)
  }
  if (is.list(x)) return(x)
  list()
}

#' Freeze a reactiveValues element (no-op stub for compatibility)
#' @param x reactiveValues object
#' @param name Element name
#' @export
freezeReactiveValue <- function(x, name) {
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Conservative invalidation
# ---------------------------------------------------------------------------

# Re-run computed *value* nodes and push updates to clients. hotShiny tracks
# dependencies statically; for state changes the static analysis cannot
# attribute to a precise edge (reactiveVal / reactiveValues writes), we
# conservatively re-evaluate reactive and render nodes.
#
# The refresh is DEFERRED to the next event-loop tick rather than run inline,
# for two reasons:
#   1. Correctness: a reactiveVal/reactiveValues write usually happens inside an
#      observer that is itself running during an executor pass, while the
#      triggering input is still marked dirty. Refreshing inline would re-enter
#      execute() and fire that observer a second time (double increments).
#      Deferring runs the refresh after the current pass clears its dirty flags,
#      so observers are not re-triggered.
#   2. Coalescing: several writes in one observer collapse into a single flush.
#
# Observers are deliberately NOT marked dirty during the flush: re-running
# side-effecting observers on every value change would fire them spuriously and
# could recurse. They still run through normal input-driven dirty propagation.
.invalidation_state <- new.env(parent = emptyenv())

.hotshiny_flush <- function() {
  executor <- tryCatch(get_executor(), error = function(e) NULL)
  if (is.null(executor)) return(invisible(NULL))
  graph <- tryCatch(executor$graph, error = function(e) NULL)
  if (!is.null(executor$builder)) {
    g <- tryCatch(executor$builder$get_graph(), error = function(e) NULL)
    if (!is.null(g)) graph <- g
  }
  if (is.null(graph)) return(invisible(NULL))
  for (node in graph$get_all_nodes()) {
    type <- tryCatch(node$type, error = function(e) NULL)
    if (!is.null(type) && type %in% c("reactive", "render")) {
      executor$state_manager$mark_dirty(node$id)
    }
  }
  tryCatch(executor$execute(), error = function(e) NULL)
  tryCatch(executor$send_output_updates(), error = function(e) NULL)
  invisible(NULL)
}

.hotshiny_invalidate_all <- function() {
  if (is.null(tryCatch(get_executor(), error = function(e) NULL))) {
    return(invisible(NULL))
  }
  .invalidation_state$pending <- TRUE
  if (isTRUE(.invalidation_state$scheduled)) {
    return(invisible(NULL))
  }
  .invalidation_state$scheduled <- TRUE
  later::later(function() {
    .invalidation_state$scheduled <- FALSE
    if (!isTRUE(.invalidation_state$pending)) return(invisible(NULL))
    .invalidation_state$pending <- FALSE
    .hotshiny_flush()
  }, delay = 0)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# reactiveVal()
# ---------------------------------------------------------------------------

#' Create a single reactive value
#'
#' Returns a function: call with no arguments to read, with one argument to set.
#'
#' @param value Initial value
#' @param label Optional label
#' @export
reactiveVal <- function(value = NULL, label = NULL) {
  builder <- get_graph_builder()
  if (is.null(builder)) {
    stop("reactiveVal() must be called within a hotShiny app context")
  }

  # Use a dedicated, unique input-style node so the executor treats it as a
  # leaf value (read from the state manager, never recomputed).
  rv_name <- paste0(".rv_", if (!is.null(label)) label else "", "_", .new_rv_id())
  node <- builder$register_input(rv_name)
  node_id <- node$id

  # Seed the initial value.
  executor <- tryCatch(get_executor(), error = function(e) NULL)
  if (!is.null(executor) && !is.null(value)) {
    executor$state_manager$set_value(node_id, value)
  }

  getset <- function(x) {
    ex <- tryCatch(get_executor(), error = function(e) NULL)
    if (missing(x)) {
      if (is.null(ex)) return(value)
      v <- ex$state_manager$get_value(node_id)
      return(v)
    }
    # set
    if (!is.null(ex)) {
      ex$state_manager$set_value(node_id, x)
      ex$state_manager$mark_dirty(node_id)
      .hotshiny_invalidate_all()
    } else {
      value <<- x
    }
    invisible(x)
  }
  attr(getset, "node_id") <- node_id
  class(getset) <- c("ReactiveValProxy", "function")
  getset
}

.rv_id_counter <- new.env(parent = emptyenv())
.new_rv_id <- function() {
  n <- if (exists("n", envir = .rv_id_counter, inherits = FALSE)) {
    get("n", envir = .rv_id_counter, inherits = FALSE)
  } else {
    0L
  }
  n <- n + 1L
  assign("n", n, envir = .rv_id_counter)
  n
}

# ---------------------------------------------------------------------------
# eventReactive() / bindEvent()
# ---------------------------------------------------------------------------

#' Create a reactive that only updates when an event expression fires
#'
#' @param eventExpr Expression whose change triggers recomputation
#' @param valueExpr Expression producing the value
#' @param label Optional label
#' @param ignoreNULL Ignore NULL/empty event values
#' @param ignoreInit Skip the initial run
#' @param ... Additional arguments (ignored)
#' @export
eventReactive <- function(eventExpr, valueExpr, label = NULL,
                          ignoreNULL = TRUE, ignoreInit = FALSE, ...) {
  builder <- get_graph_builder()
  if (is.null(builder)) {
    stop("eventReactive() must be called within a hotShiny app context")
  }

  event_ast <- rlang::get_expr(rlang::enquo(eventExpr))
  value_ast <- rlang::get_expr(rlang::enquo(valueExpr))

  event_deps <- extract_dependencies(event_ast)
  value_deps <- extract_dependencies(value_ast)
  deps <- unique(c(event_deps, value_deps))

  node <- builder$register_reactive(
    expr = value_ast,
    name = label,
    deps = deps,
    source = get_source_location(),
    env = parent.frame()
  )
  node$metadata$event_driven <- TRUE
  node$metadata$event_expr <- expr_to_ast(event_ast)
  node$metadata$ignoreNULL <- ignoreNULL
  node$metadata$ignoreInit <- ignoreInit

  ReactiveProxy$new(node_id = node$id, builder = builder)
}

#' Take a reactive/render/observer and make it event-driven (compatibility shim)
#'
#' hotShiny re-evaluates conservatively, so bindEvent currently returns its
#' input unchanged while preserving the call for translator analysis.
#'
#' @param x A reactive, render, or observer object
#' @param ... Event expressions (recorded but not used for gating yet)
#' @param ignoreNULL,ignoreInit Standard Shiny arguments
#' @export
bindEvent <- function(x, ..., ignoreNULL = TRUE, ignoreInit = FALSE) {
  x
}

# ---------------------------------------------------------------------------
# Timers: invalidateLater / reactiveTimer
# ---------------------------------------------------------------------------

#' Schedule a reactive invalidation after a delay
#'
#' Best-effort implementation: schedules a global invalidation after `millis`
#' using the app event loop. Precision differs from Shiny (it re-runs all
#' computed nodes rather than only the calling context).
#'
#' @param millis Delay in milliseconds
#' @param session Reactive domain (unused; for signature compatibility)
#' @export
invalidateLater <- function(millis, session = getDefaultReactiveDomain()) {
  later::later(function() {
    tryCatch(.hotshiny_invalidate_all(), error = function(e) NULL)
  }, delay = millis / 1000)
  invisible(NULL)
}

#' Create a reactive timer
#'
#' @param intervalMs Interval in milliseconds
#' @param session Reactive domain (unused)
#' @return A function that, when called, returns the current tick time
#' @export
reactiveTimer <- function(intervalMs = 1000, session = getDefaultReactiveDomain()) {
  tick_env <- new.env(parent = emptyenv())
  tick_env$value <- Sys.time()
  schedule <- function() {
    later::later(function() {
      tick_env$value <- Sys.time()
      tryCatch(.hotshiny_invalidate_all(), error = function(e) NULL)
      schedule()
    }, delay = intervalMs / 1000)
  }
  schedule()
  function() tick_env$value
}

# ---------------------------------------------------------------------------
# debounce / throttle
# ---------------------------------------------------------------------------

#' Debounce a reactive expression (compatibility shim)
#'
#' Returns the reactive unchanged; rate-limiting is not yet enforced server-side.
#'
#' @param r A reactive expression
#' @param millis Debounce window (recorded, not enforced)
#' @param ... Additional arguments
#' @export
debounce <- function(r, millis, ...) {
  r
}

#' Throttle a reactive expression (compatibility shim)
#'
#' @param r A reactive expression
#' @param millis Throttle window (recorded, not enforced)
#' @param ... Additional arguments
#' @export
throttle <- function(r, millis, ...) {
  r
}
