# Graph Builder
# Transforms Shiny code into reactive graph IR

#' Reactive Graph
#'
#' Container for the reactive graph structure
ReactiveGraph <- R6::R6Class("ReactiveGraph",
  public = list(
    nodes = NULL, # Named list of nodes (id -> node)
    edges = NULL, # List of edges (from -> to)
    version = NULL,
    initialize = function(version = 1L) {
      self$nodes <- new.env(parent = emptyenv())
      self$edges <- list()
      self$version <- version
    },

    # Add a node to the graph
    add_node = function(node) {
      self$nodes[[node$id]] <- node
      # Add edges for dependencies
      for (dep_id in node$deps) {
        self$edges <- c(self$edges, list(list(from = dep_id, to = node$id)))
      }
    },

    # Get a node by ID
    get_node = function(id) {
      self$nodes[[id]]
    },

    # Get all nodes
    get_all_nodes = function() {
      # self$nodes is an environment, convert to list
      node_list <- list()
      if (!is.null(self$nodes) && is.environment(self$nodes)) {
        node_ids <- ls(envir = self$nodes, all.names = TRUE)
        for (node_id in node_ids) {
          node <- get(node_id, envir = self$nodes, inherits = FALSE)
          if (!is.null(node)) {
            node_list[[node_id]] <- node
          }
        }
      }
      node_list
    },

    # Get nodes of a specific type
    get_nodes_by_type = function(type) {
      Filter(function(n) n$type == type, self$get_all_nodes())
    },

    # Serialize graph to list (for JSON)
    to_list = function() {
      list(
        version = self$version,
        nodes = lapply(self$get_all_nodes(), function(n) n$to_list()),
        edges = self$edges
      )
    },

    # Get topological sort of nodes (for execution order)
    topological_sort = function() {
      # Build adjacency list
      adj <- new.env(parent = emptyenv())
      in_degree <- new.env(parent = emptyenv())

      all_nodes <- self$get_all_nodes()
      cat("[Graph] topological_sort: found", length(all_nodes), "nodes\n", file = stderr())

      if (length(all_nodes) == 0) {
        cat("[Graph] topological_sort: WARNING - no nodes in graph!\n", file = stderr())
        return(character(0))
      }

      # Collect all node IDs from nodes and edges
      node_ids_from_nodes <- character(0)
      for (node in all_nodes) {
        node_id <- if (is.list(node) && !is.null(node$id)) node$id else if (inherits(node, "ReactiveNode")) node$id else NULL
        if (!is.null(node_id)) {
          node_ids_from_nodes <- c(node_ids_from_nodes, node_id)
        }
      }

      # Also collect node IDs from edges (in case some nodes are referenced but not in nodes list)
      node_ids_from_edges <- character(0)
      for (edge in self$edges) {
        node_ids_from_edges <- c(node_ids_from_edges, edge$from, edge$to)
      }
      all_node_ids <- unique(c(node_ids_from_nodes, node_ids_from_edges))
      cat("[Graph] topological_sort: total unique node IDs (from nodes + edges):", length(all_node_ids), "\n", file = stderr())

      # Initialize all node IDs (including those only in edges)
      for (node_id in all_node_ids) {
        if (!exists(node_id, envir = adj, inherits = FALSE)) {
          assign(node_id, list(), envir = adj)
        }
        if (!exists(node_id, envir = in_degree, inherits = FALSE)) {
          assign(node_id, 0L, envir = in_degree)
        }
      }

      # Build graph
      cat("[Graph] topological_sort: processing", length(self$edges), "edges\n", file = stderr())
      for (edge in self$edges) {
        from <- edge$from
        to <- edge$to
        cat("[Graph] topological_sort: edge from", from, "to", to, "\n", file = stderr())

        # Add edge
        if (exists(from, envir = adj, inherits = FALSE)) {
          adj_list <- get(from, envir = adj, inherits = FALSE)
          adj_list <- c(adj_list, list(to))
          assign(from, adj_list, envir = adj)
        }

        # Increment in-degree
        if (exists(to, envir = in_degree, inherits = FALSE)) {
          deg <- get(to, envir = in_degree, inherits = FALSE)
          assign(to, deg + 1L, envir = in_degree)
        }
      }

      # Kahn's algorithm
      queue <- character(0)
      result <- character(0)

      # Find nodes with in-degree 0 (check all node IDs, not just those in all_nodes)
      cat("[Graph] topological_sort: checking", length(all_node_ids), "node IDs for in-degree 0\n", file = stderr())
      for (node_id in all_node_ids) {
        if (exists(node_id, envir = in_degree, inherits = FALSE)) {
          deg <- get(node_id, envir = in_degree, inherits = FALSE)
          cat("[Graph] topological_sort: node", node_id, "has in-degree", deg, "\n", file = stderr())
          if (deg == 0L) {
            queue <- c(queue, node_id)
            cat("[Graph] topological_sort: node", node_id, "has in-degree 0, adding to queue\n", file = stderr())
          }
        } else {
          cat("[Graph] topological_sort: WARNING - node", node_id, "not found in in_degree environment\n", file = stderr())
        }
      }

      cat("[Graph] topological_sort: initial queue size:", length(queue), "\n", file = stderr())
      if (length(queue) == 0 && length(all_node_ids) > 0) {
        cat("[Graph] topological_sort: WARNING - no nodes with in-degree 0, but we have", length(all_node_ids), "node IDs!\n", file = stderr())
        cat("[Graph] topological_sort: Node IDs:", paste(all_node_ids, collapse = ", "), "\n", file = stderr())
      }

      while (length(queue) > 0) {
        current <- queue[1]
        queue <- queue[-1]
        result <- c(result, current)

        # Process neighbors
        neighbors <- get(current, envir = adj, inherits = FALSE)
        if (is.list(neighbors)) {
          neighbors <- unlist(neighbors)
        }
        for (neighbor in neighbors) {
          deg <- get(neighbor, envir = in_degree, inherits = FALSE)
          assign(neighbor, deg - 1L, envir = in_degree)
          if (get(neighbor, envir = in_degree, inherits = FALSE) == 0L) {
            queue <- c(queue, neighbor)
          }
        }
      }

      # Check for cycles
      if (length(result) != length(all_nodes)) {
        warning("Graph contains cycles - topological sort incomplete. Expected ", length(all_nodes), " nodes, got ", length(result))
      }

      result
    }
  )
)

#' Graph Builder
#'
#' Builds reactive graph from Shiny code
GraphBuilder <- R6::R6Class("GraphBuilder",
  public = list(
    graph = NULL,
    reactive_context = NULL, # Track reactive sources

    initialize = function() {
      self$graph <- ReactiveGraph$new()
      self$reactive_context <- new.env(parent = emptyenv())
      self$reactive_context$reactive_sources <- new.env(parent = emptyenv())
    },

    # Build graph from server function
    build_from_server = function(server_func, source_info = NULL) {
      # Capture the server function's body
      body_expr <- body(server_func)

      # Create a new environment to track reactive declarations
      tracking_env <- new.env(parent = environment(server_func))

      # We'll need to intercept reactive declarations
      # This is complex - we'll use a combination of:
      # 1. AST analysis
      # 2. Runtime interception (via modified functions)

      # For now, return the graph structure
      # The actual building will happen as reactive() etc. are called
      self$graph
    },

    # Register an input node
    register_input = function(input_name, source = NULL, env = NULL) {
      node_id <- paste0("input.", input_name)

      # Check if already exists
      if (!is.null(self$graph$get_node(node_id))) {
        return(self$graph$get_node(node_id))
      }

      node <- InputNode$new(
        id = node_id,
        input_name = input_name,
        source = source,
        env = env
      )

      self$graph$add_node(node)
      node
    },

    # Register a reactive expression
    register_reactive = function(expr, name = NULL, deps = NULL, source = NULL, env = NULL) {
      # Extract dependencies if not provided
      if (is.null(deps)) {
        deps <- extract_dependencies(expr)
      }

      # Convert expression to AST
      expr_ast <- expr_to_ast(expr)

      node_id <- new_node_id("reactive")
      node <- ReactiveExprNode$new(
        id = node_id,
        deps = deps,
        expr = expr_ast,
        name = name,
        source = source,
        env = env
      )

      self$graph$add_node(node)

      # Track as reactive source if named
      if (!is.null(name)) {
        self$reactive_context$reactive_sources[[name]] <- node_id
      }

      node
    },

    # Register an observer
    register_observer = function(expr, deps = NULL, priority = 0L,
                                 once = FALSE, suspended = FALSE, source = NULL, env = NULL) {
      if (is.null(deps)) {
        deps <- extract_dependencies(expr)
      }

      expr_ast <- expr_to_ast(expr)

      node_id <- new_node_id("observe")
      node <- ObserverNode$new(
        id = node_id,
        deps = deps,
        expr = expr_ast,
        priority = priority,
        once = once,
        suspended = suspended,
        source = source,
        env = env
      )

      self$graph$add_node(node)
      node
    },

    # Register a render function
    register_render = function(render_type, output_name, expr, deps = NULL, source = NULL, env = NULL) {
      if (is.null(deps)) {
        deps <- extract_dependencies(expr)
      }

      expr_ast <- expr_to_ast(expr)

      node_id <- new_node_id("render")
      node <- RenderNode$new(
        id = node_id,
        render_type = render_type,
        output_name = output_name,
        deps = deps,
        expr = expr_ast,
        source = source,
        env = env
      )

      self$graph$add_node(node)

      # Also create output node
      output_node_id <- paste0("output.", output_name)
      output_node <- OutputNode$new(
        id = output_node_id,
        output_name = output_name,
        render_type = render_type,
        deps = list(node_id),
        source = source,
        env = env
      )
      self$graph$add_node(output_node)

      node
    },

    # Get the current graph
    get_graph = function() {
      self$graph
    }
  )
)

#' Build reactive graph from server function
#'
#' @param server_func Server function
#' @param source_info Source file information
#' @return ReactiveGraph
build_reactive_graph <- function(server_func, source_info = NULL) {
  builder <- GraphBuilder$new()
  builder$build_from_server(server_func, source_info)
  builder$get_graph()
}
