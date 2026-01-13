# Complex Shiny App Example
# Demonstrates complex reactive graph

library(hotShiny)

ui <- function() {
  hotShiny::div(
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          h1("Hot Reload - Values Preserved!"),
          h2("This is a subtitle"),
          h3("this is a 3 title"),
          h4("this is a 4 title"),
          h5("this is a 5 title"),
          h6("this is a 6 title"),
          p("this is a paragraph"),
          numericInput("a", "Value A:", value = 10),
          numericInput("b", "Value B:", value = 20),
          sliderInput("sliding", "This is a slider", 1, 10, 5),
          checkboxInput("checkit", "hello", value = FALSE),
          radioButtons("radios", "choices here", c("bread", "cheese", "beer"))
        ),
        mainPanel(
          div("middle"),
          textOutput("sum"),
          textOutput("product"),
          plotOutput("plot"),
          textOutput("isTrue"),
          textOutput("isFalse"),
          textOutput("radioOuts"),
          uiOutput("iframe"),
          uiOutput("htmlthing")
        ),
        position = c("left", "right"),
        fluid = TRUE
      ),
      div(
        "hello",
        style = "background-color:yellow; height: 50px;",
        div("bye", style="background-color:blue; width:fit-content; padding:20px;"),
        span("hello there", style="background-color:red; width:fit-content; padding:20px;")
      )
    )
}

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
