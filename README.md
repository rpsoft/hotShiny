# hotShiny

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
devtools::install_github("yourusername/hotShiny")
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

## Documentation

See the [plan file](plan) for detailed architecture and implementation notes.

## License

MIT
