# Complex Shiny App Example
# Demonstrates complex reactive graph

library(hotShiny)

ui <- function() {
  div(
    h1("Complex HotShiny App"),
    numericInput("a", "Value A:", value = 1),
    numericInput("b", "Value B:", value = 2),
    textOutput("sum"),
    textOutput("product"),
    plotOutput("plot")
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
    paste("Sum:", sum_value(), "Product:", product_value())
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
# enable_hot_reload(app_obj)
# enable_strict_mode(app_obj)
# enable_time_travel(app_obj)
