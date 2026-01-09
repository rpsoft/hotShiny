# Test IR System

test_that("node types are created correctly", {
  input_node <- InputNode$new(id = "input.a", input_name = "a")
  expect_equal(input_node$type, "input")
  expect_equal(input_node$input_name, "a")
  
  reactive_node <- ReactiveExprNode$new(
    id = "reactive_1",
    deps = list("input.a"),
    expr = list(type = "call", fn = "+", args = list())
  )
  expect_equal(reactive_node$type, "reactive")
  expect_equal(reactive_node$deps, list("input.a"))
})

test_that("graph stores nodes correctly", {
  graph <- ReactiveGraph$new()
  
  node1 <- InputNode$new(id = "input.a", input_name = "a")
  node2 <- ReactiveExprNode$new(
    id = "reactive_1",
    deps = list("input.a"),
    expr = NULL
  )
  
  graph$add_node(node1)
  graph$add_node(node2)
  
  expect_equal(length(graph$get_all_nodes()), 2)
  expect_false(is.null(graph$get_node("input.a")))
  expect_false(is.null(graph$get_node("reactive_1")))
})

test_that("graph builds edges from dependencies", {
  graph <- ReactiveGraph$new()
  
  node1 <- InputNode$new(id = "input.a", input_name = "a")
  node2 <- ReactiveExprNode$new(
    id = "reactive_1",
    deps = list("input.a"),
    expr = NULL
  )
  
  graph$add_node(node1)
  graph$add_node(node2)
  
  expect_true(length(graph$edges) > 0)
  expect_true(any(sapply(graph$edges, function(e) e$from == "input.a" && e$to == "reactive_1")))
})

test_that("topological sort orders nodes correctly", {
  graph <- ReactiveGraph$new()
  
  # Create dependency chain: input.a -> reactive_1 -> reactive_2
  node1 <- InputNode$new(id = "input.a", input_name = "a")
  node2 <- ReactiveExprNode$new(
    id = "reactive_1",
    deps = list("input.a"),
    expr = NULL
  )
  node3 <- ReactiveExprNode$new(
    id = "reactive_2",
    deps = list("reactive_1"),
    expr = NULL
  )
  
  graph$add_node(node1)
  graph$add_node(node2)
  graph$add_node(node3)
  
  sorted <- graph$topological_sort()
  
  # input.a should come before reactive_1
  idx1 <- which(sorted == "input.a")
  idx2 <- which(sorted == "reactive_1")
  expect_true(idx1 < idx2)
  
  # reactive_1 should come before reactive_2
  idx3 <- which(sorted == "reactive_2")
  expect_true(idx2 < idx3)
})

test_that("graph serialization works", {
  graph <- ReactiveGraph$new()
  
  node <- InputNode$new(id = "input.a", input_name = "a")
  graph$add_node(node)
  
  graph_list <- graph$to_list()
  expect_true(is.list(graph_list))
  expect_true("nodes" %in% names(graph_list))
  expect_true("edges" %in% names(graph_list))
  
  # Test JSON serialization
  json <- serialize_graph(graph)
  expect_true(is.character(json))
  
  # Test deserialization
  graph2 <- deserialize_graph(json)
  expect_equal(length(graph2$get_all_nodes()), 1)
})

test_that("dependency extraction finds input references", {
  expr <- quote(input$a + input$b)
  deps <- extract_dependencies(expr)
  
  expect_true(length(deps) >= 2)
  expect_true(any(grepl("input.a", deps)))
  expect_true(any(grepl("input.b", deps)))
})
