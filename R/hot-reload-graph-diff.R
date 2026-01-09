# Graph Diffing
# Compares old vs new graph versions

#' Graph Diff Result
#'
#' Result of comparing two graphs
GraphDiff <- R6::R6Class("GraphDiff",
  public = list(
    unchanged = NULL,  # List of unchanged node IDs
    modified = NULL,   # List of modified node IDs
    deleted = NULL,    # List of deleted node IDs
    added = NULL,      # List of added node IDs
    
    initialize = function() {
      self$unchanged <- list()
      self$modified <- list()
      self$deleted <- list()
      self$added <- list()
    },
    
    # Check if any changes
    has_changes = function() {
      length(self$modified) > 0 ||
      length(self$deleted) > 0 ||
      length(self$added) > 0
    },
    
    # Get summary
    summary = function() {
      list(
        unchanged = length(self$unchanged),
        modified = length(self$modified),
        deleted = length(self$deleted),
        added = length(self$added),
        has_changes = self$has_changes()
      )
    }
  )
)

#' Diff two reactive graphs
#'
#' @param old_graph Old graph version
#' @param new_graph New graph version
#' @return GraphDiff object
diff_graphs <- function(old_graph, new_graph) {
  diff_result <- GraphDiff$new()
  
  old_nodes <- old_graph$get_all_nodes()
  new_nodes <- new_graph$get_all_nodes()
  
  old_node_ids <- names(old_nodes)
  new_node_ids <- names(new_nodes)
  
  # Find added nodes
  diff_result$added <- setdiff(new_node_ids, old_node_ids)
  
  # Find deleted nodes
  diff_result$deleted <- setdiff(old_node_ids, new_node_ids)
  
  # Check unchanged and modified
  common_ids <- intersect(old_node_ids, new_node_ids)
  
  for (node_id in common_ids) {
    old_node <- old_nodes[[node_id]]
    new_node <- new_nodes[[node_id]]
    
    # Compare node hashes
    old_hash <- old_node$compute_hash()
    new_hash <- new_node$compute_hash()
    
    if (identical(old_hash, new_hash)) {
      diff_result$unchanged <- c(diff_result$unchanged, list(node_id))
    } else {
      diff_result$modified <- c(diff_result$modified, list(node_id))
    }
  }
  
  diff_result
}

#' Check if a node is unchanged between versions
#'
#' @param old_node Old node
#' @param new_node New node
#' @return TRUE if unchanged
node_unchanged <- function(old_node, new_node) {
  if (is.null(old_node) || is.null(new_node)) {
    return(FALSE)
  }
  
  old_hash <- old_node$compute_hash()
  new_hash <- new_node$compute_hash()
  
  identical(old_hash, new_hash)
}

#' Compute node content hash
#'
#' @param node ReactiveNode
#' @return Hash string
compute_node_hash <- function(node) {
  node$compute_hash()
}

#' Get nodes that depend on a given node
#'
#' @param graph ReactiveGraph
#' @param node_id Node ID
#' @return List of dependent node IDs
get_dependent_nodes <- function(graph, node_id) {
  dependent <- list()
  
  for (edge in graph$edges) {
    if (edge$from == node_id) {
      dependent <- c(dependent, list(edge$to))
    }
  }
  
  unique(unlist(dependent))
}

#' Get nodes that a node depends on
#'
#' @param graph ReactiveGraph
#' @param node_id Node ID
#' @return List of dependency node IDs
get_dependency_nodes <- function(graph, node_id) {
  node <- graph$get_node(node_id)
  if (is.null(node)) {
    return(list())
  }
  
  node$deps
}

#' Check if graph structure changed
#'
#' @param old_graph Old graph
#' @param new_graph New graph
#' @return TRUE if structure changed
graph_structure_changed <- function(old_graph, new_graph) {
  # Compare edges
  old_edges <- old_graph$edges
  new_edges <- new_graph$edges
  
  # Sort edges for comparison
  old_edges_sorted <- old_edges[order(sapply(old_edges, function(e) paste(e$from, e$to)))]
  new_edges_sorted <- new_edges[order(sapply(new_edges, function(e) paste(e$from, e$to)))]
  
  !identical(old_edges_sorted, new_edges_sorted)
}
