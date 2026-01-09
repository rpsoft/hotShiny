/**
 * Puppeteer test for reloading-app.R
 */

const puppeteer = require('puppeteer');
const fs = require('fs');
const path = require('path');

describe('Hot Reloading App', () => {
  let browser;
  let page;
  const APP_URL = 'http://localhost:3839';
  const TIMEOUT = 10000;
  // Absolute path to the R file
  const R_FILE_PATH = path.resolve(__dirname, '../examples/reloading-app.R');
  let originalContent;

  beforeAll(async () => {
    // Read original content to restore later
    originalContent = fs.readFileSync(R_FILE_PATH, 'utf8');

    browser = await puppeteer.launch({
      headless: true,
      args: ['--no-sandbox', '--disable-setuid-sandbox']
    });
    page = await browser.newPage();
  });

  afterAll(async () => {
    // Restore file content
    fs.writeFileSync(R_FILE_PATH, originalContent);
    await browser.close();
  });

  test('app updates when file changes', async () => {
    // 1. Load app
    await page.goto(APP_URL, { waitUntil: 'networkidle0', timeout: TIMEOUT });
    
    // Check initial title
    let heading = await page.$eval('h1', el => el.textContent);
    expect(heading).toContain('Reloading App');

    // 2. Modify R file (change Heading)
    // We need to wait a bit to ensure file system timestamp difference is significant if needed
    await new Promise(r => setTimeout(r, 1000));

    const newContent = originalContent.replace('h1("Reloading App")', 'h1("Updated App")');
    fs.writeFileSync(R_FILE_PATH, newContent);

    // 3. Wait for hot reload to happen (polling/websocket update)
    // The previous test suite used a polling mechanism, we can do similar.
    // We expect the PAGE to reload or the DOM to update. Since hotShiny might do full page reload or dom patch.
    // HotShiny seems to use websocket updates.
    
    // Wait for the heading to change
    let updated = false;
    for (let i = 0; i < 20; i++) {
        await page.waitForTimeout(500);
        try {
            heading = await page.$eval('h1', el => el.textContent);
            if (heading.includes('Updated App')) {
                updated = true;
                break;
            }
        } catch (e) {}
    }

    expect(updated).toBe(true);

  }, 20000);
});
