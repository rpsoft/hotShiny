#!/bin/bash

# Start R app in background
echo "Starting reloading app..."
# Go to project root
cd ../../
Rscript tests/examples/reloading-app.R > tests/puppeteer/reload-app.log 2>&1 &
R_PID=$!
# Return to test dir
cd tests/puppeteer

echo "App started with PID: $R_PID"

# Wait for app to be ready
echo "Waiting for app to be ready..."
sleep 5

# Run Puppeteer tests
echo "Running Puppeteer tests..."
npx jest reloading-app.test.js
TEST_EXIT_CODE=$?

# Clean up
echo "Killing app process..."
kill $R_PID

exit $TEST_EXIT_CODE
