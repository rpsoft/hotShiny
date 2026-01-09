const puppeteer = require('puppeteer');
const { spawn } = require('child_process');
const path = require('path');
const fs = require('fs');

let appProcess = null;
let browser = null;
let page = null;

async function startApp() {
  return new Promise((resolve, reject) => {
    const appScript = path.join(__dirname, '../../tests/examples/complex-app.R');
    console.log('Starting R app from:', appScript);
    
    appProcess = spawn('Rscript', [appScript], {
      cwd: path.join(__dirname, '../..'),
      stdio: ['ignore', 'pipe', 'pipe']
    });

    let output = '';
    appProcess.stdout.on('data', (data) => {
      output += data.toString();
      if (output.includes('Server will start') || output.includes('listening')) {
        console.log('App appears to be starting...');
      }
    });

    appProcess.stderr.on('data', (data) => {
      const msg = data.toString();
      console.log('[R stderr]', msg.trim());
    });

    // Wait a bit for server to start
    setTimeout(() => {
      console.log('App process started, waiting for server...');
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

async function testPlot() {
  try {
    console.log('\n=== Starting Plot Rendering Test ===\n');

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
      if (text.includes('plot') || text.includes('Plot') || text.includes('ERROR') || text.includes('hotShiny') || text.includes('value_update')) {
        console.log('[Browser Console]', text);
      }
    });

    // Capture WebSocket messages
    await page.evaluateOnNewDocument(() => {
      window.wsMessages = [];
      // Intercept WebSocket messages
      const originalWebSocket = window.WebSocket;
      window.WebSocket = function(...args) {
        const ws = new originalWebSocket(...args);
        const originalOnMessage = ws.onmessage;
        ws.onmessage = function(event) {
          try {
            const data = JSON.parse(event.data);
            window.wsMessages.push(data);
          } catch (e) {
            // Not JSON, ignore
          }
          if (originalOnMessage) {
            originalOnMessage.call(this, event);
          }
        };
        return ws;
      };
    });

    // Navigate to app
    console.log('Navigating to http://localhost:3838...');
    await page.goto('http://localhost:3838', {
      waitUntil: 'networkidle0',
      timeout: 10000
    });

    // Wait for page to load
    await new Promise(resolve => setTimeout(resolve, 2000));

    // Check if plot element exists
    console.log('\n--- Checking for plot element ---');
    const plotElement = await page.$('#plot');
    if (!plotElement) {
      console.log('ERROR: Plot element (#plot) not found in DOM');
      const html = await page.content();
      const plotMatches = html.match(/plot/gi);
      console.log('Found "plot" in HTML', plotMatches ? plotMatches.length : 0, 'times');
      return false;
    }
    console.log('✓ Plot element found');

    // Check plot element content
    const plotContent = await page.evaluate(() => {
      const el = document.getElementById('plot');
      if (!el) return null;
      return {
        innerHTML: el.innerHTML,
        textContent: el.textContent,
        hasImg: el.querySelector('img') !== null,
        imgSrc: el.querySelector('img') ? el.querySelector('img').src.substring(0, 50) : null,
        classList: Array.from(el.classList)
      };
    });

    console.log('Plot element content:', JSON.stringify(plotContent, null, 2));

    // Wait a bit more for any async updates
    console.log('Waiting for plot to render...');
    await new Promise(resolve => setTimeout(resolve, 5000));

    // Check again after waiting
    const plotContentAfter = await page.evaluate(() => {
      const el = document.getElementById('plot');
      if (!el) return null;
      const img = el.querySelector('img');
      return {
        hasImg: img !== null,
        imgSrc: img ? img.src.substring(0, 100) : null,
        imgComplete: img ? img.complete : false,
        imgNaturalWidth: img ? img.naturalWidth : 0,
        textContent: el.textContent
      };
    });

    console.log('\n--- Plot status after waiting ---');
    console.log(JSON.stringify(plotContentAfter, null, 2));

    // Check WebSocket messages
    const wsMessages = await page.evaluate(() => {
      return window.wsMessages || [];
    });

    console.log('\n--- WebSocket messages (filtered) ---');
    const plotMessages = wsMessages.filter(m => 
      m.type === 'value_update' && 
      (m.data?.output_name === 'plot' || m.data?.outputName === 'plot')
    );
    plotMessages.forEach((msg, i) => {
      const value = msg.data?.value || '';
      const preview = value.length > 100 ? value.substring(0, 100) + '...' : value;
      console.log(`Message ${i + 1}: value length=${value.length}, preview="${preview}"`);
    });

    // Final check
    const hasPlotImage = plotContentAfter.hasImg && 
                        plotContentAfter.imgSrc && 
                        plotContentAfter.imgSrc.startsWith('data:image/');
    
    if (hasPlotImage) {
      console.log('\n✓ SUCCESS: Plot image is rendered!');
      return true;
    } else {
      console.log('\n✗ FAIL: Plot image is NOT rendered');
      console.log('  - Has img element:', plotContentAfter.hasImg);
      console.log('  - Img src starts with data:image/:', 
                  plotContentAfter.imgSrc ? plotContentAfter.imgSrc.startsWith('data:image/') : false);
      console.log('  - Text content:', plotContentAfter.textContent);
      return false;
    }

  } catch (error) {
    console.error('Test error:', error);
    return false;
  } finally {
    if (browser) {
      await browser.close();
    }
    await stopApp();
  }
}

// Run the test
testPlot()
  .then(success => {
    process.exit(success ? 0 : 1);
  })
  .catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
