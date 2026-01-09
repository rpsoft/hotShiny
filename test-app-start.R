# Test script to start app and check graph
devtools::load_all()
library(hotShiny)

ui <- function() {
  div(
    h1("Test"),
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

# Check graph before runApp
cat("=== BEFORE runApp ===\n")
graph <- app_obj$get_graph()
nodes <- graph$get_all_nodes()
cat("Nodes:", length(nodes), "\n")

# Start app (this will execute server and build graph)
cat("\n=== Starting app (will run for 10 seconds) ===\n")
app_obj$runApp(port = 3838)
