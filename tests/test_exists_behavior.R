
# Test script to verify exists() behavior

# Setup
ui <- "global_ui"
temp_env <- new.env(parent = globalenv())
server <- function() {}
assign("server", server, envir = temp_env)

# Check
cat("Checking 'ui' in temp_env (default which is inherits=TRUE):\n")
exists_ui <- exists("ui", envir = temp_env)
print(exists_ui)

cat("Checking 'ui' in temp_env (inherits=FALSE):\n")
exists_ui_strict <- exists("ui", envir = temp_env, inherits = FALSE)
print(exists_ui_strict)

cat("Checking 'server' in temp_env (default):\n")
exists_server <- exists("server", envir = temp_env)
print(exists_server)

cat("Checking 'server' in temp_env (inherits=FALSE):\n")
exists_server_strict <- exists("server", envir = temp_env, inherits = FALSE)
print(exists_server_strict)
