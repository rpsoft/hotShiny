#!/bin/bash
# Script to test the app with Puppeteer and provide feedback

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "=== Testing hotShiny Basic App ==="
echo "Project root: $PROJECT_ROOT"
echo ""

# Check if app is already running
if curl -s http://localhost:3838 > /dev/null 2>&1; then
  echo "App is already running on port 3838"
  echo "Testing with Puppeteer..."
  cd "$SCRIPT_DIR"
  node simple-test.js
  exit $?
fi

# Start the app in background
echo "Starting R app..."
cd "$PROJECT_ROOT"
Rscript tests/examples/basic-app.R &
APP_PID=$!

echo "App started with PID: $APP_PID"
echo "Waiting for app to be ready..."

# Wait for app to be ready
for i in {1..30}; do
  if curl -s http://localhost:3838 > /dev/null 2>&1; then
    echo "✓ App is ready!"
    break
  fi
  if [ $i -eq 30 ]; then
    echo "✗ ERROR: App did not start within 30 seconds"
    kill $APP_PID 2>/dev/null || true
    exit 1
  fi
  sleep 1
done

# Run Puppeteer test
echo ""
echo "Running Puppeteer test..."
cd "$SCRIPT_DIR"
node simple-test.js
TEST_EXIT_CODE=$?

# Cleanup
echo ""
echo "Stopping app (PID: $APP_PID)..."
kill $APP_PID 2>/dev/null || true
wait $APP_PID 2>/dev/null || true

exit $TEST_EXIT_CODE
