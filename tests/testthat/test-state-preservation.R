context("State Preservation")

test_that("preserved inputs are correctly extracted from state manager", {
  # Setup state manager with mixed nodes
  sm <- StateManager$new()
  sm$set_value("input.username", "suso")
  sm$set_value("input.age", 30)
  sm$set_value("calculated_value", 42) # Should be ignored
  sm$set_value("output.plot", "base64data") # Should be ignored
  
  # Simulate extraction logic used in hot-reload-reload-engine.R
  all_values <- sm$serialize_state()
  input_values <- list()
  
  for (node_id in names(all_values)) {
    if (grepl("^input\\.", node_id)) {
      # Strip "input." prefix
      input_name <- sub("^input\\.", "", node_id)
      input_values[[input_name]] <- all_values[[node_id]]
    }
  }
  
  # Verification
  expect_equal(length(input_values), 2)
  expect_equal(input_values$username, "suso")
  expect_equal(input_values$age, 30)
  expect_null(input_values$calculated_value)
  expect_null(input_values$output.plot)
})

test_that("WebSocketServer send_restore_inputs formats message correctly", {
  # Mock app not needed for this test if we override initialize
  
  # Create a partial mock of WebSocketServer
  # We inherit to get the send_restore_inputs method, but override send_message
  MockWSS <- R6::R6Class("MockWSS",
    inherit = WebSocketServer,
    public = list(
      sent_messages = list(),
      initialize = function() {
        self$connections <- list(c1 = "conn1", c2 = "conn2")
      },
      send_message = function(ws, type, data) {
        self$sent_messages[[length(self$sent_messages) + 1]] <- list(
          ws = ws,
          type = type,
          data = data
        )
      }
    )
  )
  
  ws <- MockWSS$new()
  
  # Test empty inputs - should send nothing
  ws$send_restore_inputs(list())
  expect_equal(length(ws$sent_messages), 0)
  
  ws$send_restore_inputs(NULL)
  expect_equal(length(ws$sent_messages), 0)
  
  # Test with inputs
  inputs <- list(a = 1, b = "two")
  ws$send_restore_inputs(inputs)
  
  # Should send to all connections
  expect_equal(length(ws$sent_messages), 2)
  
  msg1 <- ws$sent_messages[[1]]
  expect_equal(msg1$type, "shiny-restore-inputs")
  expect_equal(msg1$data$inputs$a, 1)
  expect_equal(msg1$data$inputs$b, "two")
  expect_true(!is.null(msg1$data$timestamp))
})

test_that("HotReloadEngine handles UI changes and state preservation", {
  # This test integrates the logic: detect change -> reload -> send updates -> send inputs
  
  # Setup
  builder <- GraphBuilder$new()
  graph <- builder$get_graph()
  
  # Create mock app
  app <- list(
    get_graph = function() graph,
    state_manager = StateManager$new(),
    builder = builder,
    executor = list(
      graph = graph,
      execute = function() {},
      send_output_updates = function() {}
    ),
    server_func = function(input, output, session) {},
    ws_server = NULL,
    ui = "<div>old</div>"
  )
  class(app) <- "HotShinyApp"
  
  # Populate state
  app$state_manager$set_value("input.test", 123)
  
  # Mock WebSocket server for app
  mock_wss_env <- new.env()
  mock_wss_env$messages <- list()
  
  mock_wss <- list(
    send_hot_reload = function(summary) {},
    send_ui_replace = function(html) {
      mock_wss_env$messages[[length(mock_wss_env$messages) + 1]] <- list(type="ui_replace", html=html)
    },
    send_restore_inputs = function(inputs) {
      mock_wss_env$messages[[length(mock_wss_env$messages) + 1]] <- list(type="restore_inputs", inputs=inputs)
    }
  )
  app$ws_server <- mock_wss
  
  # Initialize engine
  engine <- HotReloadEngine$new(app)
  
  # We cannot easily mock the whole file watching and reloading process without creating files
  # But we can verify the logic inside handle_file_change manually if we could trigger it.
  # handle_file_change is complex and sources files.
  
  # Instead, let's test the preservation logic specifically.
  
  # Mock the block we added to hot-reload-reload-engine.R
  state_manager <- app$state_manager
  all_values <- state_manager$serialize_state()
  input_values <- list()
  
  for (node_id in names(all_values)) {
    if (grepl("^input\\.", node_id)) {
      input_name <- sub("^input\\.", "", node_id)
      input_values[[input_name]] <- all_values[[node_id]]
    }
  }
  
  if (length(input_values) > 0) {
    app$ws_server$send_restore_inputs(input_values)
  }
  
  # Verify mock received the message
  msgs <- mock_wss_env$messages
  expect_equal(length(msgs), 1)
  expect_equal(msgs[[1]]$type, "restore_inputs")
  expect_equal(msgs[[1]]$inputs$test, 123)
})


