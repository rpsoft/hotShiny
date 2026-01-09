/**
 * Simple Puppeteer test that can be run directly
 * Usage: node simple-test.js
 */

const puppeteer = require('puppeteer');

(async () => {
  console.log('Launching browser...');
  const browser = await puppeteer.launch({
    headless: false, // Set to true for headless mode
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });
  
  const page = await browser.newPage();
  
  // Track console messages and errors
  const errors = [];
  const messages = [];
  
  page.on('console', msg => {
    const text = msg.text();
    const type = msg.type();
    messages.push({ type, text });
    if (type === 'error') {
      errors.push(text);
      console.error(`[BROWSER ERROR] ${text}`);
    } else {
      console.log(`[BROWSER ${type.toUpperCase()}] ${text}`);
    }
  });
  
  page.on('pageerror', error => {
    errors.push(error.message);
    console.error(`[PAGE ERROR] ${error.message}`);
  });
  
  console.log('Navigating to http://localhost:3838...');
  try {
    await page.goto('http://localhost:3838', { 
      waitUntil: 'networkidle0',
      timeout: 10000 
    });
    console.log('Page loaded successfully');
  } catch (error) {
    console.error('Failed to load page:', error.message);
    await browser.close();
    process.exit(1);
  }
  
  // Check for main elements
  console.log('Checking for UI elements...');
  try {
    const heading = await page.$eval('h1', el => el.textContent);
    console.log(`✓ Found heading: "${heading}"`);
  } catch (error) {
    console.error('✗ Heading not found:', error.message);
  }
  
  // Find and interact with input
  console.log('Looking for input field...');
  // Try multiple selectors for the input
  const inputSelectors = [
    'input[data-input-id="name"]',
    'input#name',
    'input[name="name"]',
    'input[type="text"]'
  ];
  
  let inputSelector = null;
  for (const selector of inputSelectors) {
    try {
      await page.waitForSelector(selector, { timeout: 2000 });
      inputSelector = selector;
      console.log(`✓ Input field found with selector: ${selector}`);
      break;
    } catch (e) {
      // Try next selector
    }
  }
  
  if (!inputSelector) {
    console.error('✗ Input field not found with any selector');
    // List all inputs for debugging
    const allInputs = await page.$$eval('input', inputs => 
      inputs.map(inp => ({
        id: inp.id,
        name: inp.name,
        type: inp.type,
        dataInputId: inp.getAttribute('data-input-id'),
        outerHTML: inp.outerHTML.substring(0, 100)
      }))
    );
    console.log('Available inputs:', JSON.stringify(allInputs, null, 2));
  } else {
    // Clear the input first
    await page.click(inputSelector, { clickCount: 3 });
    await page.keyboard.press('Backspace');
    
    // Type in the input
    console.log('Typing "Alice" in input field...');
    await page.type(inputSelector, 'Alice', { delay: 50 });
    
    // Wait for reactive update (give it time to process)
    console.log('Waiting for reactive update...');
    await page.waitForTimeout(2000);
    
    // Check output - try multiple selectors
    const outputSelectors = [
      '[data-output-id="greeting"]',
      '#greeting',
      '.shiny-text-output',
      'div[id="greeting"]'
    ];
    
    let outputFound = false;
    for (const selector of outputSelectors) {
      try {
        await page.waitForSelector(selector, { timeout: 2000 });
        const outputText = await page.$eval(selector, el => el.textContent.trim());
        console.log(`✓ Output found with selector "${selector}": "${outputText}"`);
        
        if (outputText.includes('Alice') && outputText.includes('Hello')) {
          console.log('✓✓✓ SUCCESS: Reactive update worked! Output contains "Alice" and "Hello"');
          outputFound = true;
          break;
        } else {
          console.log(`⚠ Output found but content doesn't match expected: "${outputText}"`);
        }
      } catch (e) {
        // Try next selector
      }
    }
    
    if (!outputFound) {
      // List all potential output elements for debugging
      const allOutputs = await page.$$eval('div, span', elements => 
        elements
          .filter(el => el.id === 'greeting' || el.getAttribute('data-output-id') === 'greeting')
          .map(el => ({
            id: el.id,
            dataOutputId: el.getAttribute('data-output-id'),
            className: el.className,
            textContent: el.textContent.trim().substring(0, 50),
            outerHTML: el.outerHTML.substring(0, 150)
          }))
      );
      console.log('Potential output elements:', JSON.stringify(allOutputs, null, 2));
    }
    
    // Test second update
    console.log('\nTesting second update: Clearing and typing "Bob"...');
    await page.click(inputSelector, { clickCount: 3 });
    await page.keyboard.press('Backspace');
    await page.type(inputSelector, 'Bob', { delay: 50 });
    await page.waitForTimeout(2000);
    
    // Check updated output
    for (const selector of outputSelectors) {
      try {
        const outputText = await page.$eval(selector, el => el.textContent.trim());
        console.log(`✓ Updated output (${selector}): "${outputText}"`);
        if (outputText.includes('Bob') && outputText.includes('Hello')) {
          console.log('✓✓✓ SUCCESS: Second reactive update worked!');
        }
        break;
      } catch (e) {
        // Try next selector
      }
    }
  }
  
  // Check for errors
  console.log('\n=== Error Summary ===');
  const inputNameErrors = errors.filter(err => 
    err.includes("input.name") || 
    err.includes("object 'input.name' not found") ||
    err.includes("Server error")
  );
  
  if (inputNameErrors.length > 0) {
    console.error(`✗ FAILED: Found ${inputNameErrors.length} "input.name" errors:`);
    inputNameErrors.forEach(err => console.error(`  - ${err}`));
  } else {
    console.log('✓ No "input.name" errors found');
  }
  
  if (errors.length > 0 && inputNameErrors.length === 0) {
    console.warn(`⚠ Found ${errors.length} other errors (not input.name related)`);
    errors.forEach(err => console.warn(`  - ${err}`));
  }
  
  if (errors.length === 0) {
    console.log('✓ No errors found in browser console');
  }
  
  console.log('\nTest complete. Browser will stay open for 5 seconds...');
  await page.waitForTimeout(5000);
  
  await browser.close();
  console.log('Browser closed.');
  
  // Exit with error code if we found input.name errors
  process.exit(inputNameErrors.length > 0 ? 1 : 0);
})();
