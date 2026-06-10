# hotShiny

https://shiny.posit.co/r/reference/shiny/1.6.0/

A Shiny-compatible R package that supports hot-reloading by transforming Shiny's execution model into a declarative, diffable reactive graph. Maintains API compatibility while fundamentally changing runtime semantics.

## Features

- **Hot Reload**: Preserve application state while reloading code changes
- **Graph-Based Reactivity**: Reactive expressions are represented as a versioned, diffable graph
- **Shiny-Compatible API**: Most Shiny apps work without modification
- **Modern Client**: Vanilla JavaScript client with DOM diffing
- **Strict Mode**: Detect side effects and non-deterministic code
- **Time-Travel Debugging**: Replay reactivity and inspect graph history

## Installation

```r
# Install from source
devtools::install_github("rpsoft/hotShiny")

#or
pak::pak("rpsoft/hotShiny") 
```

## Quick Start

```r
library(hotShiny)

ui <- function() {
  div(
    h1("Hello HotShiny"),
    textInput("name", "Name:"),
    textOutput("greeting")
  )
}

server <- function(input, output, session) {
  greeting <- reactive({
    paste("Hello,", input$name)
  })
  
  output$greeting <- renderText({
    greeting()
  })
}

app_obj <- app(ui, server)

# Enable hot reload (development)
enable_hot_reload(app_obj)

# Run app
app_obj$runApp()
```

## Architecture

hotShiny transforms Shiny code into a reactive graph:

1. **Graph Builder**: Parses Shiny code and builds a reactive graph
2. **IR System**: Represents reactive nodes as data (not closures)
3. **Runtime Executor**: Executes the graph in correct order
4. **Hot Reload Engine**: Diffs graph versions and preserves state
5. **Client Library**: Vanilla JS client with DOM diffing

## Compatibility

hotShiny supports most Shiny patterns:

- ✅ Standard reactive expressions
- ✅ `observe()` and `observeEvent()`
- ✅ All `render*()` functions
- ✅ `reactiveValues()`
- ✅ Standard UI components

Some patterns are not supported:

- ❌ Non-deterministic side effects
- ❌ Meta-programming (`get()`, `assign()`)
- ❌ Global environment mutation (`<<-`)
- ❌ Dynamic output assignment

### Unsupported Pattern Examples

**Non-deterministic side effects** - Code that produces different results on re-execution:

```r
# ❌ Avoid: Random values without seed
output$random <- renderText({

  sample(1:100, 1)  # Different result each time
})

# ❌ Avoid: System time dependencies
output$timestamp <- renderText({
  Sys.time()  # Changes on hot reload
})
```

**Meta-programming** - Dynamic variable access breaks dependency tracking:

```r
# ❌ Avoid: Dynamic variable access
server <- function(input, output, session) {
  output$result <- renderText({
    var_name <- input$selected_var
    get(var_name, envir = parent.frame())  # Dependencies not tracked
  })
}

# ❌ Avoid: Dynamic assignment
observe({
  assign(paste0("value_", input$id), input$value, envir = .GlobalEnv)
})
```

**Global environment mutation** - Superassignment creates hidden state:

```r
# ❌ Avoid: Global state mutation
counter <- 0
server <- function(input, output, session) {
  observe({
    counter <<- counter + 1  # Hidden state, lost on hot reload
  })
}
```

**Dynamic output assignment** - Outputs must be statically defined:

```r
# ❌ Avoid: Dynamic output creation
server <- function(input, output, session) {
  observe({
    output[[paste0("plot_", input$id)]] <- renderPlot({
      plot(1:10)
    })
  })
}
```

## Development Features

### Hot Reload

```r
app_obj <- app(ui, server)
enable_hot_reload(app_obj, watch_paths = c("app.R", "server.R"))
```

### Strict Mode

```r
enable_strict_mode(app_obj)
# Warns about side effects and non-deterministic code
```

### Time-Travel Debugging

```r
enable_time_travel(app_obj)
# Record and replay reactivity
```

### Debug Logging

By default, hotShiny produces minimal console output. To enable verbose debug logging:

**Server-side (R):**

```r
# Enable verbose logging before running app
options(hotshiny.verbose = TRUE)
app_obj$runApp()
```

This will show detailed logs for:
- Graph building and dependency extraction
- Hot reload operations
- Reactive execution
- WebSocket communication

**Client-side (Browser):**

```javascript
// In browser console
window.HOTSHINY_DEBUG = true;
```

This enables detailed client-side logging for:
- WebSocket messages
- DOM updates
- Input/output handling
- Hot reload events

## Documentation

See the [plan file](plan) for detailed architecture and implementation notes.

## License

MIT
