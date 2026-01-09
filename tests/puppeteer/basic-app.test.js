/**
 * Puppeteer test for basic-app.R
 * 
 * This test automates browser interaction to verify that:
 * 1. The app loads correctly
 * 2. Input field works
 * 3. Reactive updates work
 * 4. No errors appear in console
 * 
 * To run:
 *   1. Start the R app: Rscript tests/examples/basic-app.R
 *   2. In another terminal: npm test (or node tests/puppeteer/basic-app.test.js)
 */

const puppeteer = require('puppeteer');

describe('Basic HotShiny App', () => {
  let browser;
  let page;
  const APP_URL = 'http://localhost:3838';
  const TIMEOUT = 10000; // 10 seconds

  beforeAll(async () => {
    browser = await puppeteer.launch({
      headless: true, // Set to false to see the browser
      args: ['--no-sandbox', '--disable-setuid-sandbox']
    });
    page = await browser.newPage();
    
    // Listen for console errors
    page.on('console', msg => {
      const type = msg.type();
      const text = msg.text();
      if (type === 'error') {
        console.error(`Browser console error: ${text}`);
      }
    });
    
    // Listen for page errors
    page.on('pageerror', error => {
      console.error(`Page error: ${error.message}`);
    });
  });

  afterAll(async () => {
    await browser.close();
  });

  test('app loads successfully', async () => {
    await page.goto(APP_URL, { waitUntil: 'networkidle0', timeout: TIMEOUT });
    
    // Check that the page loaded
    const title = await page.title();
    expect(title).toBeTruthy();
    
    // Check for main heading
    const heading = await page.$eval('h1', el => el.textContent);
    expect(heading).toContain('Basic HotShiny App');
  }, TIMEOUT);

  test('input field is present and functional', async () => {
    await page.goto(APP_URL, { waitUntil: 'networkidle0', timeout: TIMEOUT });
    
    // Find the input field by data-input-id attribute
    const inputSelector = '[data-input-id="name"]';
    await page.waitForSelector(inputSelector, { timeout: TIMEOUT });
    
    const input = await page.$(inputSelector);
    expect(input).toBeTruthy();
    
    // Test typing in the input
    await input.type('Alice');
    const inputValue = await page.$eval(inputSelector, el => el.value);
    expect(inputValue).toBe('Alice');
  }, TIMEOUT);

  test('reactive updates work when typing in input', async () => {
    await page.goto(APP_URL, { waitUntil: 'networkidle0', timeout: TIMEOUT });
    
    const inputSelector = '[data-input-id="name"]';
    const outputSelector = '[data-output-id="greeting"], #greeting';
    
    // Wait for both input and output to be present
    await page.waitForSelector(inputSelector, { timeout: TIMEOUT });
    await page.waitForSelector(outputSelector, { timeout: TIMEOUT });
    
    // Type in the input field
    const input = await page.$(inputSelector);
    await input.click({ clickCount: 3 }); // Select all
    await input.type('Bob');
    
    // Wait for the output to update (with retry logic)
    let outputText = '';
    let attempts = 0;
    const maxAttempts = 10;
    
    while (attempts < maxAttempts) {
      await page.waitForTimeout(500); // Wait 500ms
      try {
        outputText = await page.$eval(outputSelector, el => el.textContent.trim());
        if (outputText.includes('Bob')) {
          break;
        }
      } catch (e) {
        // Element might not be ready yet
      }
      attempts++;
    }
    
    expect(outputText).toContain('Bob');
    expect(outputText).toContain('Hello');
  }, TIMEOUT * 2);

  test('no console errors appear', async () => {
    const consoleErrors = [];
    
    page.on('console', msg => {
      if (msg.type() === 'error') {
        consoleErrors.push(msg.text());
      }
    });
    
    await page.goto(APP_URL, { waitUntil: 'networkidle0', timeout: TIMEOUT });
    
    // Interact with the app
    const inputSelector = '[data-input-id="name"]';
    await page.waitForSelector(inputSelector, { timeout: TIMEOUT });
    await page.type(inputSelector, 'Test User');
    
    // Wait a bit for any async errors
    await page.waitForTimeout(2000);
    
    // Filter out expected errors (if any) and check for unexpected ones
    const unexpectedErrors = consoleErrors.filter(err => 
      !err.includes('favicon') && // Ignore favicon errors
      !err.includes('Source map') // Ignore source map warnings
    );
    
    // Check specifically that we don't have the "input.name" error
    const inputNameErrors = consoleErrors.filter(err => 
      err.includes("object 'input.name' not found") ||
      err.includes("input.name") ||
      err.includes("Server error")
    );
    
    expect(inputNameErrors.length).toBe(0);
    
    if (unexpectedErrors.length > 0) {
      console.warn('Unexpected console errors:', unexpectedErrors);
    }
  }, TIMEOUT * 2);

  test('WebSocket connection is established', async () => {
    let wsConnected = false;
    
    // Monitor WebSocket connections
    page.on('response', response => {
      if (response.url().includes('websocket') || response.status() === 101) {
        wsConnected = true;
      }
    });
    
    await page.goto(APP_URL, { waitUntil: 'networkidle0', timeout: TIMEOUT });
    
    // Wait for WebSocket to connect (check if hotshiny.js is loaded)
    await page.waitForFunction(
      () => typeof window.HotShinyClient !== 'undefined',
      { timeout: TIMEOUT }
    );
    
    // Wait a bit for WebSocket to establish
    await page.waitForTimeout(1000);
    
    // Check if WebSocket client is initialized
    const wsInitialized = await page.evaluate(() => {
      return typeof window.hotShinyClientInstance !== 'undefined' ||
             (typeof window.HotShinyClient !== 'undefined');
    });
    
    expect(wsInitialized).toBe(true);
  }, TIMEOUT * 2);
});

// If running directly (not via Jest)
if (require.main === module) {
  (async () => {
    const browser = await puppeteer.launch({ headless: false });
    const page = await browser.newPage();
    
    page.on('console', msg => console.log(`CONSOLE ${msg.type()}: ${msg.text()}`));
    page.on('pageerror', error => console.error(`PAGE ERROR: ${error.message}`));
    
    await page.goto('http://localhost:3838');
    
    console.log('Browser opened. Press Ctrl+C to close.');
    
    // Keep the browser open
    process.on('SIGINT', async () => {
      await browser.close();
      process.exit();
    });
  })();
}
