# Graph Versioning
# Manages graph versions for hot reload

#' Graph Version Manager
#'
#' Manages versions of the reactive graph
GraphVersionManager <- R6::R6Class("GraphVersionManager",
  public = list(
    versions = NULL,  # List of graph versions
    current_version = NULL,
    
    initialize = function() {
      self$versions <- list()
      self$current_version <- 1L
    },
    
    # Save a graph version
    save_version = function(graph, metadata = list()) {
      # Get compute_graph_hash function
      # It should be available from serializer.R which is loaded before this
      ns <- asNamespace("hotShiny")
      base_path <- getwd()
      if (!file.exists(file.path(base_path, "R"))) {
        base_path <- system.file(package = "hotShiny")
      }
      load_env <- new.env(parent = ns)
      
      # Load serializer if needed
      serializer_file <- file.path(base_path, "R/ir/serializer.R")
      if (file.exists(serializer_file)) {
        sys.source(serializer_file, envir = load_env)
      }
      
      compute_hash_fn <- if (exists("compute_graph_hash", envir = load_env)) {
        get("compute_graph_hash", envir = load_env)
      } else if (exists("compute_graph_hash", envir = ns)) {
        get("compute_graph_hash", envir = ns)
      } else {
        # Fallback: simple hash
        function(g) digest::digest(list(version = g$version), algo = "sha256")
      }
      
      version_data <- list(
        version = self$current_version,
        graph = graph,
        hash = compute_hash_fn(graph),
        timestamp = Sys.time(),
        metadata = metadata
      )
      
      self$versions[[as.character(self$current_version)]] <- version_data
      self$current_version <- self$current_version + 1L
      
      version_data
    },
    
    # Get a specific version
    get_version = function(version) {
      self$versions[[as.character(version)]]
    },
    
    # Get current version
    get_current = function() {
      self$get_version(self$current_version - 1L)
    },
    
    # Get previous version
    get_previous = function() {
      if (self$current_version > 2L) {
        self$get_version(self$current_version - 2L)
      } else {
        NULL
      }
    },
    
    # Compare two versions
    compare_versions = function(v1, v2) {
      if (is.numeric(v1)) v1 <- self$get_version(v1)
      if (is.numeric(v2)) v2 <- self$get_version(v2)
      
      if (is.null(v1) || is.null(v2)) {
        return(NULL)
      }
      
      list(
        hash1 = v1$hash,
        hash2 = v2$hash,
        identical = identical(v1$hash, v2$hash),
        timestamp_diff = as.numeric(v2$timestamp - v1$timestamp, units = "secs")
      )
    }
  )
)
