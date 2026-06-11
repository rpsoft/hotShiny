#!/usr/bin/env Rscript
# Integration test: observer writes to reactiveVal / reactiveValues round-trip
# to the client, and input-driven renders still work, over the real WebSocket
# protocol. Run from the package root:
#
#   Rscript tests/integration/reactive-writes.R
#
# Exits 0 on success, 1 on failure. Kept out of testthat to avoid CI flakiness
# from background server processes; run it manually after touching the executor
# or the reactive-value invalidation path.

suppressMessages(devtools::load_all(".", quiet = TRUE))
if (!requireNamespace("websocket", quietly = TRUE)) {
  message("SKIP: 'websocket' package not installed")
  quit(status = 0)
}

port <- 8799
app_file <- tempfile(fileext = ".R")
writeLines(sprintf('
suppressMessages(devtools::load_all("%s", quiet = TRUE))
ui <- function() fluidPage(
  actionButton("go", "Increment"),
  sliderInput("bins", "bins", 1, 50, 30),
  textOutput("count"), textOutput("vals"), textOutput("slider")
)
server <- function(input, output, session) {
  counter <- reactiveVal(10)
  rv <- reactiveValues(n = 0)
  observeEvent(input$go, { counter(counter() + 1); rv$n <- rv$n + 5 })
  output$count  <- renderText({ paste("counter:", counter()) })
  output$vals   <- renderText({ paste("vals:", rv$n) })
  output$slider <- renderText({ paste("bins:", input$bins) })
}
app(ui, server)$runApp(port = %d)
', normalizePath("."), port), app_file)

srv <- callr::r_bg(function(f) source(f), args = list(app_file),
                   package = FALSE) |> tryCatch(error = function(e) NULL)
if (is.null(srv)) {
  # Fall back to a plain background process if callr is unavailable.
  srv <- processx::process$new("Rscript", app_file) |>
    tryCatch(error = function(e) NULL)
}
if (is.null(srv)) { message("SKIP: need callr or processx to launch server"); quit(status = 0) }
Sys.sleep(6)

library(websocket)
seen <- new.env(); seen$m <- list()
ws <- WebSocket$new(sprintf("ws://127.0.0.1:%d/", port), autoConnect = FALSE)
ws$onMessage(function(event) {
  msg <- jsonlite::fromJSON(event$data, simplifyVector = FALSE)
  if (identical(msg$type, "value_update")) {
    seen$m[[length(seen$m) + 1]] <- paste0(msg$data$output_name, "=", msg$data$value)
  }
})
send <- function(name, val) ws$send(jsonlite::toJSON(
  list(type = "user_input", data = list(input_name = name, value = val)), auto_unbox = TRUE))
ws$onOpen(function(event) {
  later::later(function() send("go", "1"), 0.5)
  later::later(function() send("go", "2"), 1.3)
  later::later(function() send("bins", "7"), 2.1)
})
ws$connect()
end <- Sys.time() + 6
while (Sys.time() < end) { later::run_now(0.1); Sys.sleep(0.05) }
ws$close()
tryCatch(srv$kill(), error = function(e) NULL)

msgs <- unlist(seen$m)
last <- function(prefix) { hits <- grep(prefix, msgs, value = TRUE); if (length(hits)) tail(hits, 1) else NA }

checks <- list(
  "reactiveVal write (counter 10->12)" = identical(last("^count="),  "count=counter: 12"),
  "reactiveValues write (vals 0->10)"  = identical(last("^vals="),   "vals=vals: 10"),
  "input-driven render (bins=7)"        = identical(last("^slider="), "slider=bins: 7")
)
fail <- 0
for (nm in names(checks)) {
  pass <- isTRUE(checks[[nm]])
  if (!pass) fail <- fail + 1
  cat(sprintf("[%s] %s\n", if (pass) "PASS" else "FAIL", nm))
}
if (fail > 0) { cat("\nMessages seen:\n"); print(msgs) }
quit(status = if (fail > 0) 1 else 0)
