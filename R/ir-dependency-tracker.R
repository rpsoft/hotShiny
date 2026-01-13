# Dependency Tracker
# Extracts reactive dependencies from R expressions

#' Extract reactive dependencies from an expression
#'
#' @param expr Expression to analyze
#' @param env Environment context
#' @return List of dependency identifiers (e.g., "input.a", "reactive.x")
extract_dependencies <- function(expr, env = parent.frame()) {
  deps <- list()
  visited <- new.env(parent = emptyenv())
  
  # Recursively walk the AST
  walk_ast <- function(x) {
    if (rlang::is_missing(x) || is.null(x) || length(x) == 0) {
      return(NULL)
    }
    
    # Handle different expression types
    if (rlang::is_call(x)) {
      fn <- rlang::call_name(x)
      args <- rlang::call_args(x)
      
      # Special handling for { } blocks - process all statements inside
      if (fn == "{") {
        # Process all arguments (statements) in the block
        lapply(args, walk_ast)
        return(NULL)
        return(NULL)
      }
      
      # Check for input$x pattern
      if (fn == "$" && length(args) >= 2) {
        obj <- args[[1]]
        if (rlang::is_symbol(obj) && rlang::as_string(obj) == "input") {
          input_name <- tryCatch(
            rlang::as_string(args[[2]]),
            error = function(e) NULL
          )
          if (!is.null(input_name)) {
            dep_id <- paste0("input.", input_name)
            if (!exists(dep_id, envir = visited)) {
              assign(dep_id, TRUE, envir = visited)
              deps <<- c(deps, list(dep_id))
            }
          }
        }
        # Check for reactiveValues()$x pattern
        if (rlang::is_symbol(obj)) {
          obj_name <- rlang::as_string(obj)
          # Check if it's a reactiveValues object (would need context)
          # For now, we'll track it as a potential dependency
        }
      }
      
      # Check for reactive() calls
      if (fn == "reactive" || fn == "reactiveVal" || fn == "reactiveValues") {
        # These are reactive sources, not dependencies
        # But we might want to track them
      }
      
      # Check for function calls like greeting() - these might be reactive expressions
      # Try to get the reactive context from the graph builder
      builder <- tryCatch({
        get_graph_builder()
      }, error = function(e) NULL)
      
      if (!is.null(builder)) {
        # First check reactive_sources registry
        if (!is.null(builder$reactive_context)) {
          reactive_sources <- tryCatch({
            builder$reactive_context$reactive_sources
          }, error = function(e) NULL)
          if (!is.null(reactive_sources) && is.environment(reactive_sources) && exists(fn, envir = reactive_sources, inherits = FALSE)) {
            # This is a known reactive expression
            reactive_node_id <- get(fn, envir = reactive_sources, inherits = FALSE)
            dep_id <- reactive_node_id
            if (!exists(dep_id, envir = visited)) {
              assign(dep_id, TRUE, envir = visited)
              deps <<- c(deps, list(dep_id))
            }
          }
        }
        
        # Also check the graph for ALL reactive nodes
        # Match by checking if the function name could correspond to a reactive
        # This is a fallback for when reactive wasn't registered by name
        graph <- builder$get_graph()
        all_nodes <- graph$get_all_nodes()
        reactive_nodes <- Filter(function(n) inherits(n, "ReactiveExprNode"), all_nodes)
        
        # Heuristic: If there's exactly one reactive node and we're looking for a function call with no args,
        # assume it's the dependency (common pattern: greeting <- reactive(...); output$x <- renderText({ greeting() }))
        if (length(reactive_nodes) == 1 && length(args) == 0) {
          # Single reactive node, and function call with no args - likely a match
          dep_id <- reactive_nodes[[1]]$id
          if (!exists(dep_id, envir = visited)) {
            assign(dep_id, TRUE, envir = visited)
            deps <<- c(deps, list(dep_id))
          }
        } else if (length(reactive_nodes) > 1) {
          # Multiple reactive nodes - try to match by name
          for (node in reactive_nodes) {
            # Check if node name matches
            node_name <- if (!is.null(node$name)) node$name else NULL
            if (!is.null(node_name) && node_name == fn) {
              dep_id <- node$id
              if (!exists(dep_id, envir = visited)) {
                assign(dep_id, TRUE, envir = visited)
                deps <<- c(deps, list(dep_id))
              }
              break
            }
          }
        }
      }
      
      # Recursively process arguments
      lapply(args, walk_ast)
    } else if (rlang::is_symbol(x)) {
      # Check if symbol refers to a reactive value
      sym_name <- rlang::as_string(x)
      # This would require context about what's reactive
      # For now, we'll handle this in the graph builder with more context
    } else if (is.pairlist(x) || is.list(x)) {
      for (item in x) {
        walk_ast(item)
      }
    }
  }
  
  walk_ast(expr)
  unique(unlist(deps))
}

#' Extract all reactive references from an expression
#'
#' This is a more comprehensive extraction that handles:
#' - input$x
#' - reactiveValues()$x
#' - reactive() calls
#' - Symbol references that might be reactive
extract_reactive_refs <- function(expr, reactive_context = NULL) {
  refs <- list()
  
  # Track known reactive sources
  reactive_sources <- if (!is.null(reactive_context)) {
    reactive_context$reactive_sources
  } else {
    list()
  }
  
  walk_expr <- function(x, in_call = FALSE) {
    if (is.null(x)) return(NULL)
    
    if (rlang::is_call(x)) {
      fn <- rlang::call_name(x)
      args <- rlang::call_args(x)
      
      # input$x
      if (fn == "$" && length(args) >= 2) {
        obj <- args[[1]]
        field <- tryCatch(rlang::as_string(args[[2]]), error = function(e) NULL)
        
        if (rlang::is_symbol(obj)) {
          obj_name <- rlang::as_string(obj)
          
          if (obj_name == "input" && !is.null(field)) {
            ref_id <- paste0("input.", field)
            refs <<- c(refs, list(list(type = "input", id = ref_id, name = field)))
          } else if (obj_name %in% names(reactive_sources)) {
            # It's a reactiveValues or similar
            ref_id <- paste0(obj_name, ".", field)
            refs <<- c(refs, list(list(type = "reactive", id = ref_id, 
                                     source = obj_name, name = field)))
          }
        }
      }
      
      # Recursive walk
      for (arg in args) {
        walk_expr(arg, in_call = TRUE)
      }
    } else if (rlang::is_symbol(x) && in_call) {
      # Symbol reference - check if it's a known reactive
      sym_name <- rlang::as_string(x)
      if (sym_name %in% names(reactive_sources)) {
        ref_id <- sym_name
        refs <<- c(refs, list(list(type = "reactive", id = ref_id, name = sym_name)))
      }
    }
  }
  
  walk_expr(expr)
  unique_refs <- unique(lapply(refs, function(r) r$id))
  lapply(unique_refs, function(id) {
    refs[[which(sapply(refs, function(r) r$id == id))[1]]]
  })
}

#' Convert expression to AST representation (serializable)
#'
#' @param expr R expression
#' @return List representation of AST
expr_to_ast <- function(expr) {
  if (is.null(expr)) {
    return(NULL)
  }
  
  if (rlang::is_call(expr)) {
    fn <- rlang::call_name(expr)
    args <- rlang::call_args(expr)
    
    list(
      type = "call",
      fn = fn,
      args = lapply(args, expr_to_ast)
    )
  } else if (rlang::is_symbol(expr)) {
    list(
      type = "symbol",
      name = rlang::as_string(expr)
    )
  } else if (is.atomic(expr) || is.null(expr)) {
    list(
      type = "literal",
      value = expr
    )
  } else if (is.pairlist(expr) || is.list(expr)) {
    lapply(expr, expr_to_ast)
  } else {
    list(
      type = "other",
      value = expr
    )
  }
}

#' Reconstruct expression from AST
#'
#' @param ast AST representation
#' @return R expression
ast_to_expr <- function(ast) {
  if (is.null(ast)) {
    return(NULL)
  }
  
  if (is.list(ast) && "type" %in% names(ast)) {
    if (ast$type == "call") {
      fn <- rlang::sym(ast$fn)
      args <- lapply(ast$args, ast_to_expr)
      rlang::call2(fn, !!!args)
    } else if (ast$type == "symbol") {
      rlang::sym(ast$name)
    } else if (ast$type == "literal") {
      ast$value
    } else {
      ast$value
    }
  } else if (is.list(ast)) {
    lapply(ast, ast_to_expr)
  } else {
    ast
  }
}
