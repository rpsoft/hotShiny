# Complex Shiny App Example
# Demonstrates complex reactive graph

library(hotShiny)

ui <- function() {
  hotShiny::div(
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          h1("Hot Reload Test"),
          numericInput("a", "Value A:", value = 2),
          numericInput("b", "Value B:", value = 5)
        ),
        mainPanel(
          div("middle"),
          textOutput("sum"),
          textOutput("product"),
          plotOutput("plot")
        ),
        position = c("left", "right"),
        fluid = TRUE
      ),
      div(
        "hello",
        style = "background-color:yellow; height: 50px;",
        div("bye", style="background-color:blue; width:fit-content; padding:20px;")
      )
    )
}

server <- function(input, output, session) {
  # Multiple reactive expressions
  sum_value <- reactive({
    input$a + input$b
  })
  
  product_value <- reactive({
    input$a * input$b
  })
  
  # Dependent reactive
  combined <- reactive({
    paste("Sum:", sum_value(), "Product ", product_value(), "Subtract ", sum_value() - product_value())
  })
  
  # Render outputs
  output$sum <- renderText({
    paste("Sum:", sum_value())
  })
  
  output$product <- renderText({
    paste("Product:", product_value())
  })
  
  output$plot <- renderPlot({
    # Simple plot
    plot(1:10, main = combined())
  })
  
  # Observer
  observe({
    message("Sum changed to:", sum_value())
  })
}

# Create app
app_obj <- app(ui, server)

# Enable features
#enable_hot_reload(app_obj)
# enable_strict_mode(app_obj)
# enable_time_travel(app_obj)


# Enable hot reload (in development)
# Watch the current file and its directory
# Get the path to this file
app_file <- tryCatch({
  # Try to get from sys.source context
  frames <- sys.frames()
  for (frame in frames) {
    if (exists("ofile", envir = frame, inherits = FALSE)) {
      file <- get("ofile", envir = frame, inherits = FALSE)
      if (!is.null(file) && file.exists(file)) {
        return(normalizePath(file))
      }
    }
  }
  # Fallback: use relative path
  normalizePath("tests/examples/tab-complex-app.R", mustWork = TRUE)
}, error = function(e) {
  # Final fallback
  file.path(getwd(), "tests/examples/tab-complex-app.R")
})

app_dir <- dirname(app_file)
cat("[App] Watching file:", app_file, "\n", file = stderr())
cat("[App] Watching directory:", app_dir, "\n", file = stderr())
enable_hot_reload(app_obj, watch_paths = c(app_file, app_dir))

# Run app
# The server will start and be accessible at http://localhost:3838
# Press ESC or Ctrl+C to stop the server
app_obj$runApp(port = 3838)
