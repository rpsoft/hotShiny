# shiny2hotshiny: Static Compatibility Checker and Translator
# Two entry points:
#   * shiny2hotshiny_check(path)     - lint mode: report what will / won't port
#   * shiny2hotshiny_translate(path) - rewrite mode: mechanical edits + TODOs
#
# hotShiny extracts the reactive graph statically, so some Shiny patterns cannot
# simply run. The checker classifies every finding as one of:
#   ok      - supported as-is
#   auto    - mechanically translatable (translate mode handles it)
#   manual  - needs a human rewrite (translate mode inserts a TODO)

# Function names that hotShiny does not support (kept in sync with compat-stubs.R).
.unsupported_fns <- c(
  "enableBookmarking", "setBookmarkExclude", "bookmarkButton", "onBookmark",
  "onBookmarked", "onRestore", "onRestored", "bindCache", "ExtendedTask",
  "reactivePoll", "reactiveFileReader", "exportTestValues", "snapshotExclude",
  "markRenderFunction", "runExample", "runGist", "runGitHub", "runUrl"
)

# Functions that work but with reduced precision under the conservative runtime.
.partial_fns <- c(
  "reactiveVal", "reactiveValues", "invalidateLater", "reactiveTimer",
  "debounce", "throttle", "bindEvent"
)

# Functions that are mechanically translatable.
.deprecated_fns <- list(
  renderDataTable = "Prefer DT::renderDT() / DT::DTOutput() for full features."
)

#' Find R source files for an app path (file or directory)
#' @keywords internal
.app_source_files <- function(path) {
  if (dir.exists(path)) {
    files <- list.files(path, pattern = "[.][Rr]$", full.names = TRUE)
    # Prioritise the canonical Shiny files.
    order_key <- match(basename(files), c("global.R", "ui.R", "server.R", "app.R"))
    files[order(order_key, na.last = TRUE)]
  } else if (file.exists(path)) {
    path
  } else {
    stop("Path not found: ", path)
  }
}

# Build a finding record.
.finding <- function(file, line, severity, code, message) {
  data.frame(
    file = basename(file), line = line, severity = severity,
    code = code, message = message, stringsAsFactors = FALSE
  )
}

#' Scan a single file, returning a data frame of findings
#' @keywords internal
.scan_file <- function(file) {
  src <- tryCatch(parse(file, keep.source = TRUE), error = function(e) {
    return(structure("parse-error", message = conditionMessage(e)))
  })
  if (is.character(src) && identical(src[1], "parse-error")) {
    return(.finding(file, NA_integer_, "manual", "parse_error",
                    paste("Could not parse file:", attr(src, "message"))))
  }
  pd <- utils::getParseData(src)
  findings <- list()
  add <- function(...) findings[[length(findings) + 1]] <<- .finding(file, ...)

  if (!is.null(pd) && nrow(pd) > 0) {
    calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL", ]
    for (i in seq_len(nrow(calls))) {
      nm <- calls$text[i]
      ln <- calls$line1[i]
      if (nm %in% .unsupported_fns) {
        add(ln, "manual", "unsupported_fn",
            sprintf("%s() is not supported in hotShiny.", nm))
      } else if (nm %in% .partial_fns) {
        add(ln, "manual", "partial_fn",
            sprintf("%s() works but with reduced precision (conservative re-execution).", nm))
      } else if (nm %in% names(.deprecated_fns)) {
        add(ln, "auto", "deprecated_fn",
            sprintf("%s(): %s", nm, .deprecated_fns[[nm]]))
      } else if (nm == "library" || nm == "require") {
        # Checked below via text.
      }
    }
  }

  # Text-level heuristics (precise line numbers, simple to reason about).
  lines <- tryCatch(readLines(file, warn = FALSE), error = function(e) character(0))
  for (i in seq_along(lines)) {
    l <- lines[i]
    if (grepl("library\\(\\s*[\"']?shiny[\"']?\\s*\\)", l) ||
        grepl("require\\(\\s*[\"']?shiny[\"']?\\s*\\)", l)) {
      add(i, "auto", "library_swap",
          "library(shiny) -> library(hotShiny).")
    }
    # Dynamic input/output access: input[[ <expression> ]] (not a plain string).
    if (grepl("\\b(input|output)\\[\\[", l) &&
        !grepl("\\b(input|output)\\[\\[\\s*[\"'][^\"']+[\"']\\s*\\]\\]", l)) {
      add(i, "manual", "dynamic_id",
          "Computed input/output id (input[[expr]]) is invisible to static dependency tracking. List the ids explicitly.")
    }
    # Reactives created inside an apply/loop.
    if (grepl("\\b(lapply|sapply|vapply|map|for)\\b", l)) {
      # Look ahead a few lines for a reactive/observer/render created in the body.
      window <- paste(lines[i:min(i + 8, length(lines))], collapse = " ")
      if (grepl("\\b(reactive|observe|observeEvent|render[A-Z]\\w*)\\s*\\(", window)) {
        add(i, "manual", "loop_reactive",
            "Reactive/observer/output created inside a loop or apply: dependencies cannot be extracted statically.")
      }
    }
    # session features that are not supported.
    if (grepl("session\\$registerDataObj", l)) {
      add(i, "manual", "session_feature",
          "session$registerDataObj() is not supported.")
    }
  }

  if (length(findings) == 0) {
    return(.finding(file, NA_integer_, "ok", "ok", "No compatibility issues detected.")[0, ])
  }
  do.call(rbind, findings)
}

#' Check a Shiny app for hotShiny compatibility
#'
#' Parses the app's R files and reports patterns that are supported, that the
#' translator can rewrite automatically, or that require a manual rewrite.
#'
#' @param path Path to an app file or directory
#' @return A `shiny2hotshiny_report` data frame (invisibly), printed as a summary
#' @export
shiny2hotshiny_check <- function(path) {
  files <- .app_source_files(path)
  reports <- lapply(files, .scan_file)
  report <- do.call(rbind, reports)
  if (is.null(report)) report <- .scan_file(files[1])[0, ]
  class(report) <- c("shiny2hotshiny_report", class(report))
  print(report)
  invisible(report)
}

#' @export
print.shiny2hotshiny_report <- function(x, ...) {
  cat("\nshiny2hotshiny compatibility report\n")
  cat("===================================\n")
  real <- x[!is.na(x$line) | x$severity != "ok", , drop = FALSE]
  if (nrow(real) == 0) {
    cat("✓ No compatibility issues detected. This app should run under hotShiny.\n\n")
    return(invisible(x))
  }
  sev_order <- c(manual = 1, auto = 2, ok = 3)
  real <- real[order(sev_order[real$severity], real$file, real$line), , drop = FALSE]
  marks <- c(manual = "✗ MANUAL", auto = "→ AUTO  ", ok = "✓ OK    ")
  for (i in seq_len(nrow(real))) {
    r <- real[i, ]
    loc <- if (is.na(r$line)) r$file else sprintf("%s:%s", r$file, r$line)
    cat(sprintf("%s  %-28s  %s\n", marks[[r$severity]], loc, r$message))
  }
  n_manual <- sum(real$severity == "manual")
  n_auto <- sum(real$severity == "auto")
  cat(sprintf("\nSummary: %d auto-translatable, %d need manual review.\n",
              n_auto, n_manual))
  if (n_auto > 0) {
    cat("Run shiny2hotshiny_translate(path, out) to apply the automatic rewrites.\n")
  }
  cat("\n")
  invisible(x)
}

#' Translate a Shiny app file toward hotShiny
#'
#' Applies the mechanical rewrites (library swap) and inserts `# TODO(hotShiny)`
#' comments above lines that need a manual rewrite. Non-destructive: writes to
#' `out` (defaults to alongside the input with a `.hotshiny.R` suffix).
#'
#' @param path Path to a single app R file
#' @param out Output file path (default: input with `.hotshiny.R` suffix)
#' @return The output path (invisibly)
#' @export
shiny2hotshiny_translate <- function(path, out = NULL) {
  if (dir.exists(path)) {
    stop("shiny2hotshiny_translate() operates on a single file; pass an app file.")
  }
  if (is.null(out)) {
    out <- sub("[.][Rr]$", ".hotshiny.R", path)
    if (identical(out, path)) out <- paste0(path, ".hotshiny.R")
  }
  lines <- readLines(path, warn = FALSE)
  report <- .scan_file(path)

  # Index manual findings by line so we can prepend TODO comments.
  todos <- report[report$severity == "manual" & !is.na(report$line), , drop = FALSE]
  todo_by_line <- split(todos$message, todos$line)

  out_lines <- character(0)
  for (i in seq_along(lines)) {
    key <- as.character(i)
    if (!is.null(todo_by_line[[key]])) {
      indent <- sub("[^ \t].*$", "", lines[i])
      for (msg in todo_by_line[[key]]) {
        out_lines <- c(out_lines, paste0(indent, "# TODO(hotShiny): ", msg))
      }
    }
    l <- lines[i]
    # Mechanical rewrites.
    l <- gsub("library\\(\\s*[\"']?shiny[\"']?\\s*\\)", "library(hotShiny)", l)
    l <- gsub("require\\(\\s*[\"']?shiny[\"']?\\s*\\)", "require(hotShiny)", l)
    out_lines <- c(out_lines, l)
  }

  writeLines(out_lines, out)
  message("Wrote translated app to: ", out)
  message("Review any '# TODO(hotShiny)' comments before running.")
  invisible(out)
}
