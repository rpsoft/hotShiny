# hotShiny ↔ Shiny Compatibility

This document describes how compatible hotShiny is with Shiny today, what works,
what fails gracefully, and the architectural limits that the `shiny2hotshiny`
translator exists to bridge.

The north star: you should be able to replace `library(shiny)` with
`library(hotShiny)` and have an app either **work** or **fail loudly with
guidance** — never silently do the wrong thing.

## Compatibility levels

| Level | Status | Notes |
|-------|--------|-------|
| Syntax compatibility | ✅ | `reactive()`, `observe()`, `render*()`, UI tags, inputs, layouts |
| Behavioural equivalence | ⚠️ Mostly | Some edge cases differ (see Limitations) |
| Execution semantics | ❌ By design | hotShiny builds a static, diffable reactive graph |

hotShiny keeps Shiny's **API surface** while changing the **runtime** — the same
trade-off React→Preact or Vue→Solid make. That is what enables hot reload, graph
diffing and time-travel debugging.

## What works

### App entry points
- `app(ui, server)` (native) and `shinyApp(ui, server)` (alias)
- `shinyServer()`, `shinyUI()` (legacy constructors)
- `runApp("app.R")` and `runApp("appdir/")`
- `stopApp()`

### Reactivity
- `reactive()`, `observe()`, `observeEvent()`
- `isolate()` — fully supported (dependency tracking is suppressed statically)
- `req()`, `validate()` / `need()`, `isTruthy()`, `safeError()` — silent-error
  semantics; a failed `req()` blanks the output as in Shiny
- `eventReactive()` — supported (see precision note under Limitations)
- `reactiveValues()`, `reactiveVal()` — full read **and write**, including
  writes from inside observers (e.g. the counter pattern
  `observeEvent(input$go, { rv(rv() + 1) })`)
- `reactiveValuesToList()`, `is.reactive()`, `freezeReactiveValue()`
- User-defined helper functions and variables declared in the server body are
  reachable from reactive/render/observer expressions (the evaluation
  environment is parented to the captured server closure)

### The `session` object (third server argument)
Previously `NULL`; now a real object implementing:
- `session$ns()` (modules)
- `session$sendCustomMessage()` / `sendInputMessage()`
- `session$userData`, `session$clientData`
- `session$onSessionEnded()`, `onFlush()`, `onFlushed()`
- `session$reload()`, `session$setInputValue()`
- `getDefaultReactiveDomain()` returns the active session

This also fixed hotShiny's own `showModal()`, `showNotification()`, `insertUI()`
etc., which all call `session$sendCustomMessage()`.

### Modules
- `NS()`, `moduleServer()`, `callModule()`
- A namespace becomes an id prefix (`mod-x`), matching Shiny

### Inputs
- `input$x` is `NULL` until the client reports it (was `""`)
- Values are restored to natural R types: `"TRUE"`→logical, `"30"`→numeric
- Multi-value inputs return vectors: `checkboxGroupInput`,
  `selectInput(multiple = TRUE)`, `sliderInput` ranges
- `registerInputHandler()` / `removeInputHandler()` registry

### Static assets & the browser-side `Shiny` object
- `addResourcePath()` / `removeResourcePath()` serve files (htmlwidgets etc.)
- `shiny-compat.js` exposes a browser `Shiny` object:
  `Shiny.setInputValue`, `Shiny.addCustomMessageHandler`,
  `Shiny.inputBindings`/`outputBindings.register`, `Shiny.bindAll`/`unbindAll`,
  and `shiny:connected` / `shiny:sessioninitialized` / `shiny:bound` events

### Utilities
- `parseQueryString()`, `shinyOptions()`, `getShinyOption()`, `onStop()`

## Fails gracefully (catchable `hotshiny_unsupported` error)

These raise an informative error rather than `could not find function`:
bookmarking (`enableBookmarking`, `bookmarkButton`, `onBookmark`, …),
`bindCache`, `ExtendedTask`, `reactivePoll`, `reactiveFileReader`,
`exportTestValues`, `markRenderFunction`, `runExample`/`runGist`/`runGitHub`/`runUrl`.

## Known limitations (and why)

hotShiny extracts the reactive graph **statically** from the expression AST.
Patterns whose dependencies only exist at runtime cannot be analysed:

1. **Computed input/output ids** — `input[[paste0("x", i)]]`. Literal-string
   subscripts (`input[["x"]]`) are tracked; computed ones are not. Enumerate the
   ids explicitly.
2. **Reactives/observers/outputs created in loops or `lapply`** over runtime
   data — their dependencies cannot be extracted.
3. **Precision of reactive-value invalidation** — writes to
   `reactiveVal`/`reactiveValues` work (including from observers), but because
   their *reads* are not detected as static dependency edges, hotShiny refreshes
   conservatively: on any such write it re-evaluates **all** reactive and render
   nodes (correct, less precise than Shiny's targeted invalidation). Observers
   are not re-run by this pass, so an observer that only *reads* a reactive value
   (without an `input$`/event dependency) will not re-fire on that value's change.
4. **Timers** (`invalidateLater`, `reactiveTimer`) re-run all computed nodes
   rather than only the calling context.
5. **`debounce`/`throttle`/`bindCache`** are currently pass-through shims (no
   rate-limiting / caching yet).
6. **Client protocol** is hotShiny's own; ecosystem JS works via `shiny-compat.js`.
   Note hotShiny ships **Bootstrap 5** while classic Shiny markup targets
   Bootstrap 3 — `shinydashboard` and some `bslib` theming are not yet supported.

## The translator: `shiny2hotshiny`

Where a pattern can't just run, find it ahead of time:

```r
# Lint mode — classify every finding as ok / auto / manual, with file:line
shiny2hotshiny_check("path/to/app")        # file or directory

# Rewrite mode — apply mechanical edits, insert `# TODO(hotShiny):` comments
shiny2hotshiny_translate("app.R", "app.hotshiny.R")
```

`check()` reports: `library(shiny)` swaps (auto), unsupported functions (manual),
computed ids (manual), loop-created reactives (manual), reduced-precision
constructs (manual). `translate()` swaps the library call and annotates the
manual sites in place.

## Roadmap

- [ ] Depend on `htmltools` for tags + `htmlDependency` resolution (replace the
      hand-rolled tag layer; auto-inject widget dependencies)
- [ ] Precise invalidation for `reactiveVal`/`reactiveValues`
- [ ] Real `debounce`/`throttle`/`bindCache`
- [ ] Per-client sessions (currently one shared app-scoped session)
- [ ] Bootstrap 3 markup mode for `shinydashboard`
- [ ] Compatibility scoreboard: run Shiny's bundled example apps unmodified
