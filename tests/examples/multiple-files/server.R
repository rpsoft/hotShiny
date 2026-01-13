# Complex Shiny App Example
# Demonstrates complex reactive graph

server <- function(input, output, session) {
  # Multiple reactive expressions
  sum_value <- reactive({
    input$a + input$b + 10.89
  })
  
  product_value <- reactive({
    input$a * input$b
  })
  
  # Dependent reactive
  combined <- reactive({
    paste("Sums:", sum_value(), "Product ", product_value(), "Subtract ", sum_value() - product_value())
  })
  
  # Render outputs
  output$sum <- renderText({
    paste("Sum:", sum_value())
  })
  
  output$product <- renderText({
    paste("Product:", product_value())
  })
  
  output$isTrue <- renderText({
    paste("is:", input$checkit)
  })
  
  output$isFalse <- renderText({
    paste("is not:", input$sliding)
  })
  
  output$plot <- renderPlot({
    # Simple plot
    plot(1:input$sliding, main = "hello there")
  })
  
  output$radioOuts <- renderText({
    input$radios
  })

  output$iframe <- renderUI({
    if (input$checkit) {
      iframe( src= "http://wikipedia.org", width = "100%", height = "500px" )
    } else {
      NULL
    }
  })

  output$htmlthing <- renderUI({
    div(
      span("hello there", style="background-color:red; width:fit-content; padding:20px;"),  
      span("hello there", style="background-color:blue; width:fit-content; padding:20px;"),  
      span("hello there", style="background-color:green; width:fit-content; padding:20px;"),  
      style = "background-color:yellow; height: 50px;"
    )
  })
  
  # Observer
  observe({
    message("Sum changed to:", sum_value())
  })
}
