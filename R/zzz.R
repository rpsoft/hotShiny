# Package initialization
# This file is loaded last (per Collate field in DESCRIPTION)

# Debug logging - only prints when hotshiny.verbose option is TRUE
log_debug <- function(...) {
 if (isTRUE(getOption("hotshiny.verbose", FALSE))) {
    cat(..., "\n", sep = "")
  }
}

# Info logging - always prints important messages
log_info <- function(...) {
  message("[hotShiny] ", ...)
}

.onLoad <- function(libname, pkgname) {
  # Set default options
  op <- options()
  op.hotshiny <- list(
    hotshiny.verbose = FALSE
  )
  toset <- !(names(op.hotshiny) %in% names(op))
  if (any(toset)) options(op.hotshiny[toset])
  # Register S3 methods dynamically
  ns <- asNamespace("hotShiny")
  
  tryCatch({
    # InputProxy S3 methods
    if (exists("$.InputProxy", envir = ns, inherits = FALSE)) {
      registerS3method("$", "InputProxy", get("$.InputProxy", envir = ns))
    }
    
    # ReactiveValues S3 methods
    if (exists("$.ReactiveValues", envir = ns, inherits = FALSE)) {
      registerS3method("$", "ReactiveValues", get("$.ReactiveValues", envir = ns))
    }
    if (exists("$<-.ReactiveValues", envir = ns, inherits = FALSE)) {
      registerS3method("$<-", "ReactiveValues", get("$<-.ReactiveValues", envir = ns))
    }
    if (exists("[[.ReactiveValues", envir = ns, inherits = FALSE)) {
      registerS3method("[[", "ReactiveValues", get("[[.ReactiveValues", envir = ns))
    }
    if (exists("[[<-.ReactiveValues", envir = ns, inherits = FALSE)) {
      registerS3method("[[<-", "ReactiveValues", get("[[<-.ReactiveValues", envir = ns))
    }
    
    # OutputProxy S3 methods
    if (exists("$.OutputProxy", envir = ns, inherits = FALSE)) {
      registerS3method("$", "OutputProxy", get("$.OutputProxy", envir = ns))
    }
    if (exists("$<-.OutputProxy", envir = ns, inherits = FALSE)) {
      registerS3method("$<-", "OutputProxy", get("$<-.OutputProxy", envir = ns))
    }
    
    # shiny.tag S3 methods
    if (exists("print.shiny.tag", envir = ns, inherits = FALSE)) {
      registerS3method("print", "shiny.tag", get("print.shiny.tag", envir = ns))
    }
    if (exists("as.character.shiny.tag", envir = ns, inherits = FALSE)) {
      registerS3method("as.character", "shiny.tag", get("as.character.shiny.tag", envir = ns))
    }
    if (exists("c.shiny.tag", envir = ns, inherits = FALSE)) {
      registerS3method("c", "shiny.tag", get("c.shiny.tag", envir = ns))
    }
    
    # shiny.tag.list S3 methods
    if (exists("print.shiny.tag.list", envir = ns, inherits = FALSE)) {
      registerS3method("print", "shiny.tag.list", get("print.shiny.tag.list", envir = ns))
    }
    if (exists("as.character.shiny.tag.list", envir = ns, inherits = FALSE)) {
      registerS3method("as.character", "shiny.tag.list", get("as.character.shiny.tag.list", envir = ns))
    }
    if (exists("c.shiny.tag.list", envir = ns, inherits = FALSE)) {
      registerS3method("c", "shiny.tag.list", get("c.shiny.tag.list", envir = ns))
    }
    
    # shiny.tags S3 methods (for tags$xxx access)
    if (exists("$.shiny.tags", envir = ns, inherits = FALSE)) {
      registerS3method("$", "shiny.tags", get("$.shiny.tags", envir = ns))
    }
  }, error = function(e) {
    warning("Error registering S3 methods: ", conditionMessage(e))
  })
  
  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("hotShiny ", utils::packageVersion("hotShiny"), " loaded")
}
