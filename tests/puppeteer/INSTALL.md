# Installation Instructions

## Prerequisites

1. **Node.js and npm**: Install from https://nodejs.org/ (version 14 or higher)

2. **R and required packages**: Make sure hotShiny is installed in R

## Installation Steps

1. Navigate to the puppeteer test directory:
```bash
cd tests/puppeteer
```

2. Install npm dependencies:
```bash
npm install
```

This will install:
- `puppeteer` - Browser automation library
- `jest` - Testing framework (optional, for structured tests)

## Quick Start

### Simple Test (Recommended for first run)

1. Start the R app in one terminal:
```bash
cd /home/suso/datasky/hotShiny
Rscript tests/examples/basic-app.R
```

2. In another terminal, run the simple test:
```bash
cd tests/puppeteer
node simple-test.js
```

This will open a browser window and show you what's happening.

### Full Test Suite

1. Start the R app (same as above)

2. Run the full test suite:
```bash
cd tests/puppeteer
npm test
```

### Automated Script

Use the provided script to start the app and run tests automatically:
```bash
cd tests/puppeteer
./run-tests.sh
```

## Troubleshooting

- **Port 3838 already in use**: Make sure no other instance of the app is running
- **Tests timeout**: Increase the timeout values in the test files
- **Browser doesn't launch**: Make sure you have Chrome/Chromium installed (Puppeteer will download it automatically)
- **Module not found**: Run `npm install` in the `tests/puppeteer` directory
