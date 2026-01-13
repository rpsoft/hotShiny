# Complex Shiny App Example
# Demonstrates complex reactive graph

library(hotShiny)

ui <- fluidPage(
    # App title ----
    titlePanel("Hello Shiny!"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

      # Sidebar panel for inputs ----
      sidebarPanel(

        # Input: Slider for the number of bins ----
        sliderInput(inputId = "bins",
                    label = "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)

      ),

      # Main panel for displaying outputs ----
      mainPanel(

        # Output: Histogram ----
        plotOutput(outputId = "distPlot")

      )
    )
)

server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from the slider
    x    <- faithful$eruptions
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}
