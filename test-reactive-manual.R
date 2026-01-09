#!/usr/bin/env Rscript
# Manual test to verify reactive flow works

library(hotShiny)

# Simple app
ui <- function() {
  div(
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
cat("App created\n", file=stderr())

# Test if executor can access app
executor <- app_obj$get_executor()
if (is.null(executor)) {
  stop("Executor is NULL!")
}
cat("Executor found\n", file=stderr())

# Test if app reference is set
app_ref <- executor$get_app()
if (is.null(app_ref)) {
  stop("App reference is NULL in executor!")
}
cat("App reference found in executor\n", file=stderr())

# Test if WebSocket server exists
if (is.null(app_obj$ws_server)) {
  stop("WebSocket server is NULL!")
}
cat("WebSocket server found\n", file=stderr())

# Test setting input
cat("Testing set_input...\n", file=stderr())
executor$set_input("name", "Test")
cat("set_input completed\n", file=stderr())

# Check if value was computed
graph <- executor$graph
all_nodes <- graph$get_all_nodes()
for (node in all_nodes) {
  if (inherits(node, "RenderNode")) {
    value <- executor$get_value(node$id)
    cat("Render node", node$id, "value:", value, "\n", file=stderr())
  }
}

cat("Manual test completed\n", file=stderr())
app_obj$runApp(port = 3838)
