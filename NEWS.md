# hotShiny 0.2.0

## New features

* Support for splitting a Shiny app across multiple files, with server and UI
  definitions handled independently.
* Rendering Shiny UI inside iframes via `uiRender`, including live iframe updates.
* Broader Shiny API compatibility, including radio button inputs and improved
  reactive value recovery on reload.
* Compatibility with the `shiny.tailwind` package. Head-worthy content
  (`htmlDependency`, `tags$head()`, and the raw `<script>`/`<style>` emitted by
  `shiny.tailwind::use_tailwind()`) is now hoisted into the page `<head>`, the
  app's `www/` directory is served (for precompiled Tailwind CSS), and
  `app(ui, server, bootstrap = FALSE)` disables the built-in Bootstrap 5 to
  avoid clashing with Tailwind's preflight.

## Improvements

* Reactive flow now updates only the parts of the UI that actually changed.
* Hot reload now morphs the DOM in place instead of replacing the whole app,
  eliminating the full-page flash and preserving live input state (including
  checkboxes) across reloads.
* Refactored core to enhance Shiny compatibility.
* Fixed pre-initialisation issues so apps start up reliably.

# hotShiny 0.1.0

* Initial release: Shiny-compatible reactive framework with hot reloading backed
  by a versioned, diffable reactive graph.
