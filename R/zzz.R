# Package initialization
# This file is loaded last, so we can set up any final initialization

.onLoad <- function(libname, pkgname) {
  # Functions will be loaded from their respective files when needed

  # Manually load core files that are not picked up by R CMD INSTALL
  # because they are in subdirectories
  base_path <- getwd()
  if (!file.exists(file.path(base_path, "R"))) {
    base_path <- system.file(package = "hotShiny")
  }

  # Files to load in order
  files_to_load <- c(
    "R/ir/node-types.R",
    "R/runtime/state-manager.R",
    "R/core/reactive.R",
    "R/core/values.R"
  )

  ns <- asNamespace("hotShiny")

  for (file_rel in files_to_load) {
    file_path <- file.path(base_path, file_rel)
    if (file.exists(file_path)) {
      # Load into namespace
      sys.source(file_path, envir = ns)
    }
  }

  # Register S3 methods dynamically
  try(
    {
      if (exists("$.InputProxy", envir = ns)) {
        registerS3method("$", "InputProxy", get("$.InputProxy", envir = ns))
      }
      if (exists("$.ReactiveValues", envir = ns)) {
        registerS3method("$", "ReactiveValues", get("$.ReactiveValues", envir = ns))
        registerS3method("$<-", "ReactiveValues", get("$<-.ReactiveValues", envir = ns))
        registerS3method("[[", "ReactiveValues", get("[[.ReactiveValues", envir = ns))
        registerS3method("[[<-", "ReactiveValues", get("[[<-.ReactiveValues", envir = ns))
      }
      if (exists("$.OutputProxy", envir = ns)) {
        registerS3method("$", "OutputProxy", get("$.OutputProxy", envir = ns))
        registerS3method("$<-", "OutputProxy", get("$<-.OutputProxy", envir = ns))
      }
    },
    silent = FALSE
  )

  invisible(NULL)
}

# Helper to load a function from a file
.load_function <- function(func_name, file_path, base_path = NULL) {
  if (is.null(base_path)) {
    base_path <- getwd()
    if (!file.exists(file.path(base_path, "R"))) {
      base_path <- system.file(package = "hotShiny")
    }
  }

  ns <- asNamespace("hotShiny")
  load_env <- new.env(parent = ns)
  full_path <- file.path(base_path, file_path)

  if (file.exists(full_path)) {
    sys.source(full_path, envir = load_env)
    if (exists(func_name, envir = load_env)) {
      return(get(func_name, envir = load_env))
    }
  }
  stop("Function '", func_name, "' not found in ", file_path)
}

# Wrapper for enable_hot_reload that loads required files
enable_hot_reload <- function(app, watch_paths = NULL) {
  base_path <- getwd()
  if (!file.exists(file.path(base_path, "R"))) {
    base_path <- system.file(package = "hotShiny")
  }
  ns <- asNamespace("hotShiny")
  load_env <- new.env(parent = ns)

  # Load required files in dependency order
  files_to_load <- c(
    "R/ir/node-types.R",
    "R/ir/serializer.R",
    "R/hot-reload/file-watcher.R",
    "R/runtime/versioning.R",
    "R/hot-reload/state-preservation.R",
    "R/hot-reload/graph-diff.R",
    "R/hot-reload/reload-engine.R"
  )
  for (file_rel in files_to_load) {
    file_path <- file.path(base_path, file_rel)
    if (file.exists(file_path)) {
      sys.source(file_path, envir = load_env)
    }
  }

  if (exists("enable_hot_reload", envir = load_env)) {
    get("enable_hot_reload", envir = load_env)(app, watch_paths)
  } else {
    stop("enable_hot_reload function not found after loading files")
  }
}

# Wrapper for enable_strict_mode
enable_strict_mode <- function(app) {
  base_path <- getwd()
  if (!file.exists(file.path(base_path, "R"))) {
    base_path <- system.file(package = "hotShiny")
  }
  fn <- .load_function("enable_strict_mode", "R/core/strict-mode.R", base_path)
  fn(app)
}

# Wrapper for disable_strict_mode
disable_strict_mode <- function(app) {
  base_path <- getwd()
  if (!file.exists(file.path(base_path, "R"))) {
    base_path <- system.file(package = "hotShiny")
  }
  fn <- .load_function("disable_strict_mode", "R/core/strict-mode.R", base_path)
  fn(app)
}

# Wrapper for enable_time_travel
enable_time_travel <- function(app, max_history = 1000) {
  base_path <- getwd()
  if (!file.exists(file.path(base_path, "R"))) {
    base_path <- system.file(package = "hotShiny")
  }
  fn <- .load_function("enable_time_travel", "R/debug/time-travel.R", base_path)
  fn(app, max_history)
}

# Wrapper for disable_time_travel
disable_time_travel <- function(app) {
  base_path <- getwd()
  if (!file.exists(file.path(base_path, "R"))) {
    base_path <- system.file(package = "hotShiny")
  }
  fn <- .load_function("disable_time_travel", "R/debug/time-travel.R", base_path)
  fn(app)
}

# Wrapper for visualize_graph
visualize_graph <- function(graph, format = "dot") {
  base_path <- getwd()
  if (!file.exists(file.path(base_path, "R"))) {
    base_path <- system.file(package = "hotShiny")
  }
  fn <- .load_function("visualize_graph", "R/debug/time-travel.R", base_path)
  fn(graph, format)
}
