# Reloading App Example
# For testing hot reload functionality

library(hotShiny)

ui <- function() {
    div(
        h1("Reloading App"),
        textInput("name", "Enter your name:"),
        textOutput("greeting")
    )
}

server <- function(input, output, session) {
    # Reactive expression
    greeting <- reactive({
        paste("Hello,", input$name, "!")
    })

    # Render output
    output$greeting <- renderText({
        greeting()
    })
}

# Create and run app
app_obj <- app(ui, server)

# Enable hot reload
enable_hot_reload(app_obj)

# Run app on a different port to avoid conflict
app_obj$runApp(port = 3839)
