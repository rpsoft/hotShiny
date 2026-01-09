# Puppeteer Tests for hotShiny

This directory contains automated browser tests using Puppeteer to verify that the hotShiny app works correctly in a real browser environment.

## Setup

1. Install Node.js and npm (if not already installed)

2. Install dependencies:
```bash
cd tests/puppeteer
npm install
```

## Running Tests

### Option 1: Run with Jest (Recommended)

1. Start the R app in one terminal:
```bash
cd /home/suso/datasky/hotShiny
Rscript tests/examples/basic-app.R
```

2. In another terminal, run the tests:
```bash
cd tests/puppeteer
npm test
```

### Option 2: Run directly with Node.js (for debugging)

1. Start the R app in one terminal:
```bash
cd /home/suso/datasky/hotShiny
Rscript tests/examples/basic-app.R
```

2. In another terminal, run the test script:
```bash
cd tests/puppeteer
node basic-app.test.js
```

This will open a browser window so you can see what's happening.

## Test Coverage

The tests verify:

1. **App loads successfully** - Checks that the page loads and displays the heading
2. **Input field is functional** - Verifies the input field exists and accepts text
3. **Reactive updates work** - Tests that typing in the input updates the output
4. **No console errors** - Ensures no errors appear in the browser console (especially the "input.name" error)
5. **WebSocket connection** - Verifies that the WebSocket client is initialized

## Debugging

If tests fail:

1. Check that the R app is running on `http://localhost:3838`
2. Run with `headless: false` in the test file to see the browser
3. Check the console output for error messages
4. Increase the `TIMEOUT` value if tests are timing out

## Continuous Integration

These tests can be integrated into CI/CD pipelines. Make sure to:
- Install Node.js and npm in your CI environment
- Start the R app as a background process
- Wait for the app to be ready before running tests
