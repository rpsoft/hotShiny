const puppeteer = require('puppeteer');
const { spawn } = require('child_process');
const path = require('path');
const fs = require('fs');

let appProcess = null;
let browser = null;
let page = null;
const appFile = path.join(__dirname, '../../tests/examples/complex-app.R');

async function startApp() {
  return new Promise((resolve, reject) => {
    console.log('Starting R app from:', appFile);
    
    appProcess = spawn('Rscript', [appFile], {
      cwd: path.join(__dirname, '../..'),
      stdio: ['ignore', 'pipe', 'pipe']
    });

    let output = '';
    appProcess.stdout.on('data', (data) => {
      output += data.toString();
    });

    appProcess.stderr.on('data', (data) => {
      const msg = data.toString();
      if (msg.includes('Hot reload enabled') || msg.includes('listening')) {
        console.log('[R]', msg.trim());
      }
    });

    setTimeout(() => {
      console.log('App process started');
      resolve();
    }, 3000);
  });
}

async function stopApp() {
  if (appProcess) {
    console.log('Stopping app...');
    appProcess.kill();
    appProcess = null;
  }
}

async function modifyServerFile() {
  // Read the file
  let content = fs.readFileSync(appFile, 'utf8');
  
  // Change product_value to use multiplication instead of addition
  const original = 'product_value <- reactive({\n    input$a + input$b\n  })';
  const modified = 'product_value <- reactive({\n    input$a * input$b\n  })';
  
  if (content.includes('input$a + input$b') && content.includes('product_value')) {
    content = content.replace(
      /product_value <- reactive\(\{\s*input\$a \+ input\$b\s*\}\)/,
      'product_value <- reactive({\n    input$a * input$b\n  })'
    );
    fs.writeFileSync(appFile, content, 'utf8');
    console.log('Modified server file: changed product_value to use multiplication');
    return true;
  } else {
    console.log('File already modified or pattern not found');
    return false;
  }
}

async function restoreServerFile() {
  // Restore original
  let content = fs.readFileSync(appFile, 'utf8');
  content = content.replace(
    /product_value <- reactive\(\{\s*input\$a \* input\$b\s*\}\)/,
    'product_value <- reactive({\n    input$a + input$b\n  })'
  );
  fs.writeFileSync(appFile, content, 'utf8');
  console.log('Restored server file to original');
}

async function testHotReload() {
  try {
    console.log('\n=== Starting Hot Reload Test ===\n');

    // Start the app
    await startApp();
    await new Promise(resolve => setTimeout(resolve, 2000));

    // Launch browser
    console.log('Launching browser...');
    browser = await puppeteer.launch({
      headless: true,
      args: ['--no-sandbox', '--disable-setuid-sandbox']
    });

    page = await browser.newPage();

    // Enable console logging
    page.on('console', msg => {
      const text = msg.text();
      if (text.includes('hot_reload') || text.includes('HotReload') || text.includes('reload') || text.includes('product')) {
        console.log('[Browser Console]', text);
      }
    });

    // Navigate to app
    console.log('Navigating to http://localhost:3838...');
    await page.goto('http://localhost:3838', {
      waitUntil: 'networkidle0',
      timeout: 10000
    });

    // Wait for initial load and get initial values
    console.log('Waiting for initial load...');
    await new Promise(resolve => setTimeout(resolve, 5000));

    // Get initial product value
    console.log('\n--- Getting initial product value ---');
    const initialProduct = await page.evaluate(() => {
      const el = document.getElementById('product');
      return el ? el.textContent : null;
    });
    console.log('Initial product value:', initialProduct);
    
    // Also get sum value to verify it's working
    const initialSum = await page.evaluate(() => {
      const el = document.getElementById('sum');
      return el ? el.textContent : null;
    });
    console.log('Initial sum value:', initialSum);

    // Modify the server file
    console.log('\n--- Modifying server file ---');
    const modified = await modifyServerFile();
    if (!modified) {
      console.log('Could not modify file, test may not be valid');
    }

    // Wait for hot reload to trigger
    console.log('Waiting for hot reload...');
    await new Promise(resolve => setTimeout(resolve, 2000));

    // Check if product value changed
    console.log('\n--- Checking product value after reload ---');
    await new Promise(resolve => setTimeout(resolve, 2000));
    
    const productAfterReload = await page.evaluate(() => {
      const el = document.getElementById('product');
      return el ? el.textContent : null;
    });
    console.log('Product value after reload:', productAfterReload);

    // Check if it changed (should be multiplication now)
    // With a=1, b=2: addition gives 3, multiplication gives 2
    const initialIsAddition = initialProduct && (initialProduct.includes('3') || initialProduct.includes('Product: 3'));
    const afterIsMultiplication = productAfterReload && productAfterReload.includes('Product: 2');
    
    // Also check if sum still works (should be unchanged)
    const sumAfterReload = await page.evaluate(() => {
      const el = document.getElementById('sum');
      return el ? el.textContent : null;
    });
    console.log('Sum value after reload:', sumAfterReload);
    
    if (afterIsMultiplication && productAfterReload !== initialProduct) {
      console.log('\n✓ SUCCESS: Hot reload worked! Product changed');
      console.log('  Initial:', initialProduct);
      console.log('  After:', productAfterReload);
      console.log('  Sum (should be unchanged):', sumAfterReload);
      return true;
    } else {
      console.log('\n✗ FAIL: Hot reload may not have worked correctly');
      console.log('  Initial product:', initialProduct);
      console.log('  After product:', productAfterReload);
      console.log('  Values are different:', productAfterReload !== initialProduct);
      return false;
    }

  } catch (error) {
    console.error('Test error:', error);
    return false;
  } finally {
    // Restore file
    await restoreServerFile();
    
    if (browser) {
      await browser.close();
    }
    await stopApp();
  }
}

// Run the test
testHotReload()
  .then(success => {
    process.exit(success ? 0 : 1);
  })
  .catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
