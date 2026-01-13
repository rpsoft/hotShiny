# Complex Shiny App Example
# Demonstrates complex reactive graph

ui <- function() {
  hotShiny::div(
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          h1("Hot Reload - Values Preserved!"),
          h2("This is a subtitle"),
          h3("this is a 3 title"),
          p("this is a paragraph"),
          numericInput("a", "Value A:", value = 5),
          numericInput("b", "Value B:", value = 20),
          sliderInput("sliding", "This is a slider", 1, 10, 5),
          checkboxInput("checkit", "hello", value = FALSE),
          radioButtons("radios", "choices here", c("bread", "cheese", "beer"))
        ),
        mainPanel(
          uiOutput("iframe"),
          div("middle"),
          textOutput("sum"),
          textOutput("product"),
          plotOutput("plot"),
          textOutput("isTrue"),
          textOutput("isFalse"),
          textOutput("radioOuts"),
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
