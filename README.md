# hotShiny

https://shiny.posit.co/r/reference/shiny/1.6.0/

A Shiny-compatible R package that supports hot-reloading by transforming Shiny's execution model into a declarative, diffable reactive graph. Maintains API compatibility while fundamentally changing runtime semantics.

## Features

- **Hot Reload**: Preserve application state while reloading code changes, morphing the DOM in place so only changed nodes update (no full-page flash, inputs and scroll position are preserved)
- **Graph-Based Reactivity**: Reactive expressions are represented as a versioned, diffable graph
- **Shiny-Compatible API**: Most Shiny apps work without modification
- **Tailwind CSS Support**: First-class [`shiny.tailwind`](https://github.com/kylebutts/shiny.tailwind) compatibility, head/dependency hoisting, an opt-out for the built-in Bootstrap, and utility classes straight on inputs (e.g. `textInput("name", NULL, class = "border rounded-xl")`)
- **Modern Client**: Vanilla JavaScript client with DOM diffing
- **Strict Mode**: Detect side effects and non-deterministic code
- **Time-Travel Debugging**: Replay reactivity and inspect graph history

## Installation

```r
# Install from source
devtools::install_github("dark-peak-analytics/hotShiny")

#or
pak::pak("dark-peak-analytics/hotShiny") 
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

## Styling with Tailwind CSS

hotShiny is compatible with the [`shiny.tailwind`](https://github.com/kylebutts/shiny.tailwind)
package and adds a few conveniences for styling apps with Tailwind utility
classes.

### 1. Add utility classes to any UI function

Every input/UI function accepts extra attributes via `...`. The most useful is
`class`, which is **appended** to the element's base class rather than replacing
it — so you can attach Tailwind utilities directly, no wrapper `div` required:

```r
ui <- function() {
  div(
    textInput("name", NULL, class = "border rounded-xl w-full px-3 py-2"),
    actionButton("go", "Submit", class = "px-4 py-2 bg-blue-600 text-white rounded-lg"),
    selectInput("opt", "Choose", c("a", "b"), class = "ring-2 ring-blue-300")
  )
}
```

For single-control inputs the class lands on the control itself
(`<input>`/`<select>`/`<textarea>`); for grouped inputs (`radioButtons`,
`checkboxGroupInput`) it lands on the group container. Any other attribute
(`style`, `data-*`, `aria-*`, …) is forwarded the same way.

### 2. Quick start — browser JIT (no build step)

`use_tailwind()` loads Tailwind's in-browser JIT compiler from a CDN. hotShiny
automatically hoists its `<script>`/`<style>` into the page `<head>`, and because
hot reload only morphs the app body, the compiler keeps running and restyles new
classes instantly on every reload. Set `bootstrap = FALSE` so Tailwind's
preflight reset has a clean slate:

```r
library(hotShiny)
library(shiny.tailwind)

ui <- function() {
  div(
    use_tailwind(),  # head content is hoisted automatically
    div(class = "max-w-xl mx-auto mt-10 p-8 bg-white rounded-2xl shadow-lg",
      h1(class = "text-3xl font-bold text-blue-600", "hotShiny + Tailwind"),
      textInput("name", NULL, class = "mt-4 border rounded-lg px-3 py-2 w-full"),
      div(class = "mt-4 text-gray-700", textOutput("greeting"))
    )
  )
}

server <- function(input, output, session) {
  output$greeting <- renderText(paste0("Hello, ", input$name))
}

app_obj <- app(ui, server, bootstrap = FALSE)  # drop the built-in Bootstrap 5
enable_hot_reload(app_obj)
app_obj$runApp()
```

If `shiny.tailwind` isn't installed you can inline the same CDN script yourself
with `HTML('<script src="https://unpkg.com/@tailwindcss/browser@4"></script>')`
— hotShiny hoists raw `<script>`/`<style>` head fragments just the same.

### 3. Precompiled CSS (production)

For production you can ship a precompiled stylesheet instead of the CDN
compiler. Compile your CSS (e.g. with `shiny.tailwind::compile_tailwindcss()`)
into the app's `www/` directory — hotShiny serves `www/` like Shiny does — and
link it from `tags$head()`:

```r
ui <- function() {
  div(
    tags$head(tags$link(rel = "stylesheet", href = "tailwind.min.css")),
    h1(class = "text-3xl font-bold text-blue-600", "Hello")
  )
}

app_obj <- app(ui, server, bootstrap = FALSE)
```

`tags$head()` content and `htmlDependency()` objects are hoisted into the page
`<head>` automatically, so other dependency-based packages work too.

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
