#!/usr/bin/env Rscript
# Test WebSocket input flow

library(hotShiny)

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

# Get executor
executor <- app_obj$get_executor()
if (is.null(executor)) {
  stop("Executor is NULL!")
}

# Simulate WebSocket input
cat("Simulating input 'Alice'...\n", file=stderr())
executor$set_input("name", "Alice")
cat("Input set completed\n", file=stderr())

# Check render node value
graph <- executor$graph
all_nodes <- graph$get_all_nodes()
for (node in all_nodes) {
  if (inherits(node, "RenderNode")) {
    value <- executor$get_value(node$id)
    cat("Render node", node$id, "output_name=", node$output_name, "value='", value, "'\n", file=stderr())
    if (value != "" && grepl("Alice", value)) {
      cat("SUCCESS: Reactive flow works! Value contains 'Alice'\n", file=stderr())
    } else {
      cat("FAILURE: Value is empty or doesn't contain 'Alice'\n", file=stderr())
    }
  }
}

cat("Test completed\n", file=stderr())
