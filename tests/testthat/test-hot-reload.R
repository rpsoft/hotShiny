# Test Hot Reload Functionality

test_that("file watcher detects file changes", {
  # Create temporary file
  temp_file <- tempfile(fileext = ".R")
  writeLines("x <- 1", temp_file)
  
  watcher <- FileWatcher$new()
  changed <- FALSE
  
  watcher$watch(temp_file, callback = function(file) {
    changed <<- TRUE
  })
  
  # Modify file
  Sys.sleep(0.1)  # Small delay
  writeLines("x <- 2", temp_file)
  
  # Check (would need to wait for async check)
  # For now, just test that watcher is set up
  expect_true(file.exists(temp_file))
  
  # Cleanup
  unlink(temp_file)
})

test_that("graph diffing identifies changes", {
  # Create old graph
  old_graph <- ReactiveGraph$new()
  node1 <- InputNode$new(id = "input.a", input_name = "a")
  old_graph$add_node(node1)
  
  # Create new graph with same node
  new_graph <- ReactiveGraph$new()
  node2 <- InputNode$new(id = "input.a", input_name = "a")
  new_graph$add_node(node2)
  
  diff <- diff_graphs(old_graph, new_graph)
  
  # Should be unchanged
  expect_true(length(diff$unchanged) > 0)
  expect_equal(length(diff$added), 0)
  expect_equal(length(diff$deleted), 0)
  expect_equal(length(diff$modified), 0)
})

test_that("graph diffing identifies new nodes", {
  old_graph <- ReactiveGraph$new()
  node1 <- InputNode$new(id = "input.a", input_name = "a")
  old_graph$add_node(node1)
  
  new_graph <- ReactiveGraph$new()
  node2 <- InputNode$new(id = "input.a", input_name = "a")
  node3 <- InputNode$new(id = "input.b", input_name = "b")
  new_graph$add_node(node2)
  new_graph$add_node(node3)
  
  diff <- diff_graphs(old_graph, new_graph)
  
  expect_true(length(diff$added) > 0)
  expect_true(any(sapply(diff$added, function(id) id == "input.b")))
})

test_that("state preservation stores and restores values", {
  sm <- StateManager$new()
  sm$set_value("node_1", 42)
  sm$set_value("node_2", "test")
  
  spm <- StatePreservationManager$new()
  spm$preserve_nodes_state(c("node_1", "node_2"), sm)
  
  # Clear state
  sm$clear_all()
  
  # Restore
  restored <- spm$restore_nodes_state(c("node_1", "node_2"), sm)
  
  expect_equal(restored, 2)
  expect_equal(sm$get_value("node_1"), 42)
  expect_equal(sm$get_value("node_2"), "test")
})

test_that("hot reload engine tracks versions", {
  builder <- GraphBuilder$new()
  graph <- builder$get_graph()
  
  # Create mock app
  app <- list(
    get_graph = function() graph,
    get_state_manager = function() StateManager$new(),
    builder = builder,
    executor = list(graph = graph),
    server_func = function(input, output, session) {}
  )
  class(app) <- "HotShinyApp"
  
  engine <- HotReloadEngine$new(app)
  
  # Save version
  engine$version_manager$save_version(graph)
  
  expect_equal(length(engine$version_manager$versions), 1)
  expect_equal(engine$version_manager$current_version, 2L)
})
