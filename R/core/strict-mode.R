# Strict Mode
# Detects side effects and non-deterministic code

#' Strict Mode Checker
#'
#' Checks for side effects and non-deterministic operations
StrictModeChecker <- R6::R6Class("StrictModeChecker",
  public = list(
    enabled = NULL,
    warnings = NULL,
    errors = NULL,
    
    # Side effect patterns
    side_effect_fns = NULL,
    non_deterministic_fns = NULL,
    
    initialize = function(enabled = FALSE) {
      self$enabled <- enabled
      self$warnings <- list()
      self$errors <- list()
      
      # Functions that cause side effects
      self$side_effect_fns <- c(
        "write.csv", "write.table", "save", "saveRDS",
        "writeLines", "cat", "print", "message", "warning",
        "assign", "<<-", "read.csv", "read.table", "readRDS",
        "file.create", "file.remove", "dir.create", "unlink"
      )
      
      # Functions that are non-deterministic
      self$non_deterministic_fns <- c(
        "runif", "rnorm", "sample", "Sys.time", "Sys.Date",
        "timestamp", "date", "now"
      )
    },
    
    # Check expression for side effects
    check_expression = function(expr, context = NULL) {
      if (!self$enabled) {
        return(list(ok = TRUE, warnings = list(), errors = list()))
      }
      
      issues <- list()
      
      # Walk AST and check for problematic patterns
      self$walk_ast(expr, issues, context)
      
      # Store issues
      for (issue in issues) {
        if (issue$severity == "error") {
          self$errors <- c(self$errors, list(issue))
        } else {
          self$warnings <- c(self$warnings, list(issue))
        }
      }
      
      list(
        ok = length(issues) == 0 || all(sapply(issues, function(i) i$severity == "warning")),
        warnings = Filter(function(i) i$severity == "warning", issues),
        errors = Filter(function(i) i$severity == "error", issues)
      )
    },
    
    # Walk AST and detect issues
    walk_ast = function(expr, issues, context) {
      if (is.null(expr)) {
        return(invisible(NULL))
      }
      
      if (rlang::is_call(expr)) {
        fn <- rlang::call_name(expr)
        args <- rlang::call_args(expr)
        
        # Check for side effect functions
        if (fn %in% self$side_effect_fns) {
          issues <- c(issues, list(list(
            type = "side_effect",
            severity = "error",
            function_name = fn,
            message = paste("Side effect detected:", fn, "- not allowed in reactive expressions"),
            context = context
          )))
        }
        
        # Check for non-deterministic functions
        if (fn %in% self$non_deterministic_fns) {
          issues <- c(issues, list(list(
            type = "non_deterministic",
            severity = "warning",
            function_name = fn,
            message = paste("Non-deterministic function detected:", fn, "- may cause issues with hot reload"),
            context = context
          )))
        }
        
        # Check for assignment operators
        if (fn == "<-" || fn == "=") {
          # Check for global assignment
          if (length(args) >= 1) {
            target <- args[[1]]
            if (rlang::is_call(target) && rlang::call_name(target) == "<<-") {
              issues <- c(issues, list(list(
                type = "global_assignment",
                severity = "error",
                message = "Global assignment (<<-) detected - not allowed in reactive expressions",
                context = context
              )))
            }
          }
        }
        
        # Check for meta-programming
        if (fn == "get" || fn == "assign" || fn == "eval" || fn == "parse") {
          issues <- c(issues, list(list(
            type = "meta_programming",
            severity = "warning",
            function_name = fn,
            message = paste("Meta-programming detected:", fn, "- may not work correctly with hot reload"),
            context = context
          )))
        }
        
        # Recursively check arguments
        for (arg in args) {
          self$walk_ast(arg, issues, context)
        }
      } else if (rlang::is_symbol(expr)) {
        # Check symbol usage
        # (Could check for global variable access)
      }
    },
    
    # Get all warnings
    get_warnings = function() {
      self$warnings
    },
    
    # Get all errors
    get_errors = function() {
      self$errors
    },
    
    # Clear all issues
    clear = function() {
      self$warnings <- list()
      self$errors <- list()
    },
    
    # Enable strict mode
    enable = function() {
      self$enabled <- TRUE
    },
    
    # Disable strict mode
    disable = function() {
      self$enabled <- FALSE
    }
  )
)

#' Enable strict mode for an app
#'
#' @param app HotShinyApp instance
#' @return App with strict mode enabled
enable_strict_mode <- function(app) {
  strict_checker <- attr(app, "strict_checker")
  if (is.null(strict_checker)) {
    attr(app, "strict_checker") <- StrictModeChecker$new(enabled = TRUE)
  } else {
    strict_checker$enable()
    attr(app, "strict_checker") <- strict_checker
  }
  app
}

#' Disable strict mode for an app
#'
#' @param app HotShinyApp instance
#' @return App with strict mode disabled
disable_strict_mode <- function(app) {
  strict_checker <- attr(app, "strict_checker")
  if (!is.null(strict_checker)) {
    strict_checker$disable()
    attr(app, "strict_checker") <- strict_checker
  }
  app
}

#' Check expression in strict mode
#'
#' @param expr Expression to check
#' @param app HotShinyApp instance
#' @param context Context information
#' @return Check result
check_strict <- function(expr, app, context = NULL) {
  strict_checker <- attr(app, "strict_checker")
  if (is.null(strict_checker)) {
    return(list(ok = TRUE, warnings = list(), errors = list()))
  }
  
  strict_checker$check_expression(expr, context)
}
