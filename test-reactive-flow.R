# Quick test script to verify reactive flow
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

# Check graph before running
cat("Graph nodes before runApp:\n")
graph <- app_obj$get_graph()
all_nodes <- graph$get_all_nodes()
cat("  Total nodes:", length(all_nodes), "\n")
for (node in all_nodes) {
  cat("  Node:", node$id, "Type:", class(node)[1])
  if (inherits(node, "RenderNode")) {
    cat(" Output:", node$output_name)
  }
  cat("\n")
}

# Now run the app (this will execute server function and build graph)
# But we'll do it manually to test
cat("\nExecuting server function manually...\n")
executor <- app_obj$get_executor()
builder <- app_obj$builder

# Set graph builder and executor
set_graph_builder(builder)
set_executor(executor)

# Create input/output proxies
InputProxyClass <- get("InputProxy", envir = asNamespace("hotShiny"))
OutputProxyClass <- get("OutputProxy", envir = asNamespace("hotShiny"))

input <- InputProxyClass$new(builder = builder)
output <- if (is.function(OutputProxyClass)) {
  OutputProxyClass(builder)
} else {
  OutputProxyClass$new(builder = builder)
}
session <- NULL

# Execute server function
server(input, output, session)

# Check graph after
cat("\nGraph nodes after server execution:\n")
graph <- app_obj$get_graph()
all_nodes <- graph$get_all_nodes()
cat("  Total nodes:", length(all_nodes), "\n")
for (node in all_nodes) {
  cat("  Node:", node$id, "Type:", class(node)[1])
  if (inherits(node, "RenderNode")) {
    cat(" Output:", node$output_name, "Deps:", paste(node$deps, collapse=", "))
  } else if (inherits(node, "ReactiveExprNode")) {
    cat(" Deps:", paste(node$deps, collapse=", "))
  }
  cat("\n")
}

# Test setting input
cat("\nTesting set_input...\n")
executor$set_input("name", "Alice")

# Check values
cat("\nValues after set_input:\n")
cat("  input.name:", executor$get_value("input.name"), "\n")
cat("  reactive.greeting:", executor$get_value("reactive.greeting"), "\n")
cat("  render.greeting:", executor$get_value("render.greeting"), "\n")
