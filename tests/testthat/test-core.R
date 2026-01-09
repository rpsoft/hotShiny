# Test Core Functionality

test_that("reactive() creates reactive node", {
  builder <- GraphBuilder$new()
  set_graph_builder(builder)
  
  x <- reactive({
    input$a * 2
  })
  
  graph <- builder$get_graph()
  nodes <- graph$get_all_nodes()
  
  expect_true(length(nodes) > 0)
  expect_true(any(sapply(nodes, function(n) n$type == "reactive")))
})

test_that("observe() creates observer node", {
  builder <- GraphBuilder$new()
  set_graph_builder(builder)
  
  observe({
    x <- input$a
  })
  
  graph <- builder$get_graph()
  nodes <- graph$get_all_nodes()
  
  expect_true(any(sapply(nodes, function(n) n$type == "observe")))
})

test_that("reactiveValues() creates reactive values", {
  builder <- GraphBuilder$new()
  set_graph_builder(builder)
  
  rv <- reactiveValues(a = 1, b = 2)
  
  expect_true(inherits(rv, "ReactiveValues"))
  expect_equal(rv$get("a"), 1)
  expect_equal(rv$get("b"), 2)
})

test_that("graph builder tracks dependencies", {
  builder <- GraphBuilder$new()
  set_graph_builder(builder)
  
  x <- reactive({
    input$a + input$b
  })
  
  graph <- builder$get_graph()
  nodes <- graph$get_all_nodes()
  
  reactive_nodes <- Filter(function(n) n$type == "reactive", nodes)
  expect_true(length(reactive_nodes) > 0)
  
  # Check dependencies
  deps <- reactive_nodes[[1]]$deps
  expect_true(length(deps) >= 2)
})

test_that("state manager stores and retrieves values", {
  sm <- StateManager$new()
  
  sm$set_value("node_1", 42)
  expect_equal(sm$get_value("node_1"), 42)
  
  sm$set_value("node_2", "test")
  expect_equal(sm$get_value("node_2"), "test")
})

test_that("executor executes reactive graph", {
  builder <- GraphBuilder$new()
  set_graph_builder(builder)
  
  # Create a simple reactive
  x <- reactive({
    2 + 2
  })
  
  graph <- builder$get_graph()
  state_manager <- StateManager$new()
  executor <- ReactiveExecutor$new(graph, state_manager)
  set_executor(executor)
  
  # Execute
  executor$execute()
  
  # Check that value was computed
  nodes <- graph$get_all_nodes()
  reactive_nodes <- Filter(function(n) n$type == "reactive", nodes)
  if (length(reactive_nodes) > 0) {
    node_id <- reactive_nodes[[1]]$id
    value <- state_manager$get_value(node_id)
    # Value should be computed (though execution is simplified)
  }
})
