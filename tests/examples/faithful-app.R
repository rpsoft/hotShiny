# Complex Shiny App Example
# Demonstrates complex reactive graph

library(hotShiny)

ui <- fluidPage(
 
    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

server <- function(input, output, session) {
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins+1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             ylab = 'Frequency',
             main = paste("Histogram of waiting times"))
    })
}

# Create and run app
app_obj <- app(ui, server)

# Enable hot reload (in development)
enable_hot_reload(app_obj)

# Run app
# The server will start and be accessible at http://localhost:3838
# Press ESC or Ctrl+C to stop the server
app_obj$runApp(port = 3838)
