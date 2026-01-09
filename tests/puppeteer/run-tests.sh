#!/bin/bash
# Script to run Puppeteer tests
# This script starts the R app and runs the tests

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
TEST_DIR="$SCRIPT_DIR"

echo "Starting hotShiny app..."
cd "$PROJECT_ROOT"

# Start R app in background
Rscript tests/examples/basic-app.R > app.log 2>&1 &
APP_PID=$!

echo "App started with PID: $APP_PID"
echo "Waiting for app to be ready..."

# Wait for app to be ready (check if port 3838 is listening)
for i in {1..30}; do
  if curl -s http://localhost:3838 > /dev/null 2>&1; then
    echo "App is ready!"
    break
  fi
  if [ $i -eq 30 ]; then
    echo "ERROR: App did not start within 30 seconds"
    kill $APP_PID 2>/dev/null || true
    exit 1
  fi
  sleep 1
done

# Run tests
echo "Running Puppeteer tests..."
cd "$TEST_DIR"
npm test
TEST_EXIT_CODE=$?

# Cleanup: kill the R app
echo "Stopping app..."
kill $APP_PID 2>/dev/null || true
wait $APP_PID 2>/dev/null || true

exit $TEST_EXIT_CODE
