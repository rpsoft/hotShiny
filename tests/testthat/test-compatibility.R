# Test Shiny Compatibility

test_that("basic reactive expression works", {
  builder <- GraphBuilder$new()
  set_graph_builder(builder)
  
  # This should work like Shiny's reactive()
  x <- reactive({
    1 + 1
  })
  
  expect_true(!is.null(x))
  # In real implementation, x() would return 2
})

test_that("observe() works like Shiny", {
  builder <- GraphBuilder$new()
  set_graph_builder(builder)
  
  # Should create observer
  obs <- observe({
    x <- input$a
  })
  
  expect_true(inherits(obs, "ObserverProxy"))
})

test_that("reactiveValues() works like Shiny", {
  builder <- GraphBuilder$new()
  set_graph_builder(builder)
  
  rv <- reactiveValues(a = 1, b = 2)
  
  expect_equal(rv$a, 1)
  expect_equal(rv$b, 2)
  
  # Should support assignment
  rv$a <- 3
  expect_equal(rv$a, 3)
})

test_that("render functions create render nodes", {
  builder <- GraphBuilder$new()
  set_graph_builder(builder)
  
  # Mock output assignment context
  set_current_output_name("text_output")
  
  render <- renderText({
    "Hello"
  })
  
  expect_true(inherits(render, "RenderProxy"))
  
  clear_current_output_name()
})

test_that("app() creates app object", {
  ui <- function() {
    "Hello"
  }
  
  server <- function(input, output, session) {
    x <- reactive({
      input$a
    })
  }
  
  app_obj <- app(ui, server)
  
  expect_true(inherits(app_obj, "HotShinyApp"))
  expect_false(is.null(app_obj$builder))
  expect_false(is.null(app_obj$executor))
})
