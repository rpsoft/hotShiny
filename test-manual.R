#!/usr/bin/env Rscript
# Manual test script to verify reactive flow

library(hotShiny)

ui <- function() {
  div(
    h1("Test App"),
    textInput("name", "Name:"),
    textOutput("greeting")
  )
}

server <- function(input, output, session) {
  greeting <- reactive({
    paste("Hello,", input$name, "!")
  })
  
  output$greeting <- renderText({
    greeting()
  })
}

app_obj <- app(ui, server)
app_obj$runApp(port = 3838, host = "127.0.0.1")
