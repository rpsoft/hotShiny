# Tests for Shiny compatibility layer (R/compat-*.R, translator, dep tracking)

test_that("isTruthy matches Shiny semantics", {
  expect_false(isTruthy(NULL))
  expect_false(isTruthy(""))
  expect_false(isTruthy(FALSE))
  expect_false(isTruthy(NA))
  expect_false(isTruthy(character(0)))
  expect_true(isTruthy(0))
  expect_true(isTruthy("x"))
  expect_true(isTruthy(TRUE))
})

test_that("req() halts with a silent error on falsy input", {
  expect_identical(req(5), 5)
  err <- tryCatch(req(NULL), error = function(e) e)
  expect_s3_class(err, "shiny.silent.error")
})

test_that("need() and validate() validate inputs", {
  expect_identical(need(FALSE, "needed"), "needed")
  expect_null(need(TRUE, "needed"))
  err <- tryCatch(validate(need(FALSE, "bad")), error = function(e) e)
  expect_s3_class(err, "shiny.silent.error")
  expect_silent(validate(need(TRUE, "ok")))
})

test_that("isolate() returns its value", {
  expect_equal(isolate(1 + 1), 2)
})

test_that("NS namespaces ids", {
  ns <- NS("mod")
  expect_equal(ns("x"), "mod-x")
  expect_equal(NS("mod", "x"), "mod-x")
  expect_equal(NS("", "x"), "x")
})

test_that("parseQueryString decodes parameters", {
  q <- parseQueryString("?a=1&b=hello%20world")
  expect_equal(q$a, "1")
  expect_equal(q$b, "hello world")
  expect_equal(length(parseQueryString("")), 0L)
})

test_that("shinyOptions / getShinyOption round-trip", {
  shinyOptions(test_opt_xyz = 42)
  expect_equal(getShinyOption("test_opt_xyz"), 42)
  expect_null(getShinyOption("does_not_exist"))
})

test_that("unsupported stubs raise a catchable classed error", {
  err <- tryCatch(enableBookmarking(), error = function(e) e)
  expect_s3_class(err, "hotshiny_unsupported")
})

test_that("addResourcePath registers and resolves files", {
  dir <- tempfile()
  dir.create(dir)
  writeLines("x", file.path(dir, "f.js"))
  addResourcePath("unit_lib", dir)
  resolved <- resolve_resource_path("/unit_lib/f.js")
  expect_false(is.null(resolved))
  expect_true(file.exists(resolved))
  # Path traversal is rejected.
  expect_null(resolve_resource_path("/unit_lib/../../etc/passwd"))
  removeResourcePath("unit_lib")
})

test_that("isolate() suppresses dependency tracking", {
  deps <- extract_dependencies(quote({ input$a + isolate(input$b) }))
  expect_true("input.a" %in% deps)
  expect_false("input.b" %in% deps)
})

test_that("input[[\"literal\"]] is tracked as a dependency", {
  deps <- extract_dependencies(quote(input[["bins"]] + 1))
  expect_true("input.bins" %in% deps)
})

test_that("req(input$x) still records the dependency", {
  deps <- extract_dependencies(quote({ req(input$sel); paste(input$sel) }))
  expect_true("input.sel" %in% deps)
})

test_that("coerce_input_value restores natural types", {
  expect_identical(coerce_input_value("TRUE"), TRUE)
  expect_identical(coerce_input_value("30"), 30)
  expect_identical(coerce_input_value("hello"), "hello")
  expect_null(coerce_input_value(NULL))
  expect_identical(coerce_input_value(c("a", "b")), c("a", "b"))
})

test_that("shinyApp builds an app object", {
  ui <- function() fluidPage(textOutput("o"))
  server <- function(input, output, session) {
    output$o <- renderText({ "hi" })
  }
  a <- shinyApp(ui, server)
  expect_true(inherits(a, "HotShinyApp"))
})

test_that("moduleServer builds without error and namespaces outputs", {
  modServer <- function(id) moduleServer(id, function(input, output, session) {
    output$o <- renderText({ paste("ns", session$ns("o")) })
  })
  ui <- function() fluidPage(div("x"))
  server <- function(input, output, session) modServer("m1")
  expect_true(inherits(shinyApp(ui, server), "HotShinyApp"))
})

test_that("ShinySession ns and userData behave", {
  s <- ShinySession$new(ns_prefix = "mod")
  expect_equal(s$ns("x"), "mod-x")
  s$userData$count <- 3
  expect_equal(s$userData$count, 3)
  child <- s$makeScope("sub")
  expect_equal(child$ns("x"), "mod-sub-x")
})

test_that("shiny2hotshiny_check flags known issues", {
  app <- tempfile(fileext = ".R")
  writeLines(c(
    "library(shiny)",
    "server <- function(input, output, session) {",
    "  enableBookmarking('url')",
    "  output$o <- renderText({ input[[paste0('x', 1)]] })",
    "}"
  ), app)
  rep <- suppressWarnings(shiny2hotshiny_check(app))
  expect_s3_class(rep, "shiny2hotshiny_report")
  expect_true(any(rep$code == "library_swap"))
  expect_true(any(rep$code == "unsupported_fn"))
  expect_true(any(rep$code == "dynamic_id"))
})

test_that("shiny2hotshiny_translate swaps library and inserts TODOs", {
  app <- tempfile(fileext = ".R")
  writeLines(c(
    "library(shiny)",
    "server <- function(input, output, session) {",
    "  enableBookmarking('url')",
    "}"
  ), app)
  out <- suppressMessages(shiny2hotshiny_translate(app))
  result <- readLines(out)
  expect_true(any(grepl("library(hotShiny)", result, fixed = TRUE)))
  expect_false(any(grepl("library(shiny)", result, fixed = TRUE)))
  expect_true(any(grepl("# TODO(hotShiny)", result, fixed = TRUE)))
})
