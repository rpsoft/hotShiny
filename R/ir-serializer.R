# Graph Serializer
# Serialize/deserialize reactive graph to/from JSON

#' Serialize graph to JSON
#'
#' @param graph ReactiveGraph object
#' @param pretty Whether to pretty-print JSON
#' @return JSON string
serialize_graph <- function(graph, pretty = FALSE) {
  graph_list <- graph$to_list()
  jsonlite::toJSON(graph_list, pretty = pretty, auto_unbox = TRUE)
}

#' Deserialize graph from JSON
#'
#' @param json JSON string
#' @return ReactiveGraph object
deserialize_graph <- function(json) {
  graph_list <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  
  graph <- ReactiveGraph$new(version = if (is.null(graph_list$version)) 1L else graph_list$version)
  
  # Reconstruct nodes
  for (node_data in graph_list$nodes) {
    node <- reconstruct_node(node_data)
    graph$add_node(node)
  }
  
  # Edges are already in the list
  graph$edges <- graph_list$edges
  
  graph
}

#' Reconstruct a node from serialized data
#'
#' @param node_data List representation of node
#' @return ReactiveNode object
reconstruct_node <- function(node_data) {
  type <- node_data$type
  id <- node_data$id
  deps <- if (is.null(node_data$deps)) list() else node_data$deps
  expr <- node_data$expr
  version <- if (is.null(node_data$version)) 1L else node_data$version
  source <- node_data$source
  metadata <- if (is.null(node_data$metadata)) list() else node_data$metadata
  
  switch(type,
    "input" = {
      input_name <- if (is.null(metadata$input_name)) gsub("^input\\.", "", id) else metadata$input_name
      InputNode$new(
        id = id,
        input_name = input_name,
        version = version,
        source = source
      )
    },
    "output" = {
      output_name <- if (is.null(metadata$output_name)) gsub("^output\\.", "", id) else metadata$output_name
      render_type <- metadata$render_type
      OutputNode$new(
        id = id,
        output_name = output_name,
        render_type = render_type,
        deps = deps,
        expr = expr,
        version = version,
        source = source
      )
    },
    "reactive" = {
      name <- metadata$name
      ReactiveExprNode$new(
        id = id,
        deps = deps,
        expr = expr,
        name = name,
        version = version,
        source = source
      )
    },
    "observe" = {
      priority <- if (is.null(metadata$priority)) 0L else metadata$priority
      once <- if (is.null(metadata$once)) FALSE else metadata$once
      suspended <- if (is.null(metadata$suspended)) FALSE else metadata$suspended
      ObserverNode$new(
        id = id,
        deps = deps,
        expr = expr,
        priority = priority,
        once = once,
        suspended = suspended,
        version = version,
        source = source
      )
    },
    "render" = {
      render_type <- metadata$render_type
      output_name <- metadata$output_name
      RenderNode$new(
        id = id,
        render_type = render_type,
        output_name = output_name,
        deps = deps,
        expr = expr,
        version = version,
        source = source
      )
    },
    {
      # Default: base ReactiveNode
      ReactiveNode$new(
        id = id,
        type = type,
        deps = deps,
        expr = expr,
        version = version,
        source = source,
        metadata = metadata
      )
    }
  )
}

#' Compute graph hash for versioning
#'
#' @param graph ReactiveGraph object
#' @return Hash string
compute_graph_hash <- function(graph) {
  # Sort nodes by ID for consistent hashing
  nodes <- graph$get_all_nodes()
  node_hashes <- vapply(nodes, function(n) n$compute_hash(), character(1))
  sorted_hashes <- sort(node_hashes)
  
  content <- list(
    nodes = sorted_hashes,
    edges = graph$edges
  )
  
  digest::digest(content, algo = "sha256")
}
