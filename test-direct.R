#!/usr/bin/env Rscript
# Direct test of reactive flow

devtools::load_all()
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
cat("App created\n")

# Get executor
executor <- app_obj$get_executor()
cat("Executor obtained\n")

# Check graph before runApp
graph_before <- executor$graph
cat("Graph before runApp: ", length(graph_before$get_all_nodes()), " nodes\n")

# Manually trigger server execution to build graph
# This simulates what runApp does
input <- InputProxy$new(builder = app_obj$builder)
output <- OutputProxy(app_obj$builder)
session <- list()

# Set graph builder and executor in context
hotShiny::set_graph_builder(app_obj$builder)
hotShiny::set_executor(executor)

# Execute server function
app_obj$server(input, output, session)

# Check graph after server execution
graph_after <- app_obj$builder$get_graph()
cat("Graph after server execution: ", length(graph_after$get_all_nodes()), " nodes\n")
nodes <- graph_after$get_all_nodes()
for (n in nodes) {
  cat("  Node: ", n$id, ", deps: ", paste(n$deps, collapse=", "), "\n")
}

# Update executor's graph
executor$graph <- graph_after

# Test setting input
cat("\nTesting set_input('name', 'Alice')...\n")
executor$set_input("name", "Alice")

# Check render node value
all_nodes <- graph_after$get_all_nodes()
for (node in all_nodes) {
  if (inherits(node, "RenderNode")) {
    value <- executor$get_value(node$id)
    cat("Render node ", node$id, " (output_name=", node$output_name, ") value: '", value, "'\n")
    if (value != "" && grepl("Alice", value)) {
      cat("SUCCESS: Reactive flow works!\n")
    } else {
      cat("FAILURE: Value is empty or doesn't contain 'Alice'\n")
    }
  }
}

cat("\nTest complete\n")
