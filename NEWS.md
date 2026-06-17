# hotShiny 0.2.0

## New features

* Support for splitting a Shiny app across multiple files, with server and UI
  definitions handled independently.
* Rendering Shiny UI inside iframes via `uiRender`, including live iframe updates.
* Broader Shiny API compatibility, including radio button inputs and improved
  reactive value recovery on reload.

## Improvements

* Reactive flow now updates only the parts of the UI that actually changed.
* Refactored core to enhance Shiny compatibility.
* Fixed pre-initialisation issues so apps start up reliably.

# hotShiny 0.1.0

* Initial release: Shiny-compatible reactive framework with hot reloading backed
  by a versioned, diffable reactive graph.
