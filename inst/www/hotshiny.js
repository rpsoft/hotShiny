/**
 * HotShiny Main Client Library
 * Orchestrates client-side functionality
 */

(function (global) {
  'use strict';

  // Debug logging - set window.HOTSHINY_DEBUG = true to enable
  const DEBUG = () => window.HOTSHINY_DEBUG === true;
  const logDebug = (...args) => { if (DEBUG()) console.log('[hotShiny]', ...args); };
  const logWarn = (...args) => { if (DEBUG()) console.warn('[hotShiny]', ...args); };
  const logError = (...args) => console.error('[hotShiny]', ...args);  // Errors always show

  class HotShiny {
    constructor(options = {}) {
      this.options = {
        wsUrl: options.wsUrl || this.getWebSocketUrl(),
        autoConnect: options.autoConnect !== false,
        debug: options.debug || false,
        ...options
      };

      // Enable debug mode via option
      if (this.options.debug) {
        window.HOTSHINY_DEBUG = true;
      }

      this.wsClient = null;
      this.reactiveClient = new ReactiveClient();
      this.domPatcher = null;
      this.initialized = false;

      if (this.options.autoConnect) {
        this.init();
      }
    }

    init() {
      if (this.initialized) {
        return;
      }

      // Initialize WebSocket client
      this.wsClient = new WebSocketClient(this.options.wsUrl);

      // Register message handlers
      this.registerMessageHandlers();

      // Initialize DOM patcher
      const rootElement = document.getElementById('hotshiny-root') || document.body;
      this.domPatcher = new DOMPatcher(rootElement);

      // Wire up input event listeners
      this.setupInputListeners();

      this.initialized = true;
    }

    setupInputListeners() {
      // Wait for DOM to be ready
      const attachNow = () => {
        this.attachInputListeners();
        // Also set up a mutation observer to re-attach listeners when DOM changes
        if (!this.mutationObserver) {
          this.mutationObserver = new MutationObserver(() => {
            // Re-attach listeners when DOM changes
            this.attachInputListeners();
          });
          this.mutationObserver.observe(document.body, {
            childList: true,
            subtree: true
          });
        }
      };

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', attachNow);
      } else {
        attachNow();
      }
    }

    attachInputListeners() {
      // Find all input elements and attach listeners
      // Look for inputs with data-input-id attribute (from textInput) or regular inputs
      const inputs = document.querySelectorAll('input[data-input-id], input[type="text"], input[type="number"], input[type="email"], textarea, select');

      inputs.forEach(input => {
        // Get input ID from data-input-id attribute, id, or name
        const inputId = input.getAttribute('data-input-id') || input.id || input.name;
        if (!inputId) return;

        // Check if listener already attached (avoid duplicates)
        if (input.hasAttribute('data-hotshiny-listener')) {
          return;
        }
        input.setAttribute('data-hotshiny-listener', 'true');

        // Helper to get correct value based on input type
        const getInputValue = (el) => {
          if (el.type === 'checkbox') {
            return el.checked;
          }
          return el.value;
        };

        // Send initial value ONLY on first page load (not after hot reload)
        const initialValue = getInputValue(input);

        // Determine if we should send this value
        // For checkboxes, false is a valid value we want to send
        // For text inputs, empty string is valid but original code skipped it? 
        // Original: initialValue !== ''
        // Improved: Allow boolean false, allow empty string if it's not undefined
        const isValid = initialValue !== undefined && initialValue !== null;
        const isNotEmpty = initialValue !== '';

        if (isValid && (isNotEmpty || input.type === 'checkbox') && !this.initialValuesSent) {
          logDebug(` Sending initial value for ${inputId}: ${initialValue}`);
          // Use a small delay to ensure WebSocket is connected
          setTimeout(() => {
            this.sendInput(inputId, initialValue);
          }, 100);
        }

        // Track the last sent value to avoid redundant sends
        input.setAttribute('data-hotshiny-last-value', String(initialValue));

        // Attach input event (fires on every keystroke)
        input.addEventListener('input', (e) => {
          const value = getInputValue(e.target);
          const lastValue = input.getAttribute('data-hotshiny-last-value');
          // Only send if value actually changed
          if (String(value) !== lastValue) {
            input.setAttribute('data-hotshiny-last-value', String(value));
            this.sendInput(inputId, value);
          }
        });

        // Also attach change event (fires on blur/enter)
        input.addEventListener('change', (e) => {
          const value = getInputValue(e.target);
          const lastValue = input.getAttribute('data-hotshiny-last-value');
          // Only send if value actually changed
          if (String(value) !== lastValue) {
            input.setAttribute('data-hotshiny-last-value', String(value));
            this.sendInput(inputId, value);
          }
        });
      });

      // Mark that initial values have been sent (for first page load)
      this.initialValuesSent = true;
    }

    registerMessageHandlers() {
      // Graph update
      this.wsClient.registerHandler('graph_update', (message) => {
        this.reactiveClient.updateGraph(message.data);
        this.handleGraphUpdate(message.data);
      });

      // Value update
      this.wsClient.registerHandler('value_update', (message) => {
        const data = message.data;
        const nodeId = data.node_id || data.nodeId;
        const value = data.value;
        const outputName = data.output_name || data.outputName;

        // Update the value in reactive client
        if (nodeId) {
          this.reactiveClient.updateValue(nodeId, value);
        }

        // Handle the update (will update DOM)
        this.handleValueUpdate({ nodeId, value, outputName });
      });

      // DOM patch
      this.wsClient.registerHandler('dom_patch', (message) => {
        this.handleDOMPatch(message.data);
      });

      // Hot reload
      this.wsClient.registerHandler('hot_reload', (message) => {
        this.handleHotReload(message.data);
      });

      // Error
      this.wsClient.registerHandler('error', (message) => {
        console.error('Server error:', message.data.message);
      });

      // Remote browser() break: server hit browser() inside a render/reactive/
      // observe expression. Log the captured R-side snapshot and pause in
      // DevTools (debugger; is a no-op when DevTools is closed).
      this.wsClient.registerHandler('debug_break', (message) => {
        this.handleDebugBreak(message.data);
      });
    }

    handleDebugBreak(data) {
      if (window.hotShinyRemoteBrowser === false) return; // optional kill switch
      const label = (data && (data.label || data.node)) || '(server)';
      const values = (data && data.values) || {};
      console.group('%c⏸ hotShiny browser() @ ' + label,
                    'color:#c00;font-weight:bold');
      if (data && data.text) console.log('label:', data.text);
      console.log('R-side snapshot:', values);
      console.log('Note: this paused the browser, not R; R has already continued.');
      console.groupEnd();
      debugger; // pauses only if DevTools is open; otherwise a harmless no-op
    }

    handleGraphUpdate(graphData) {
      // Rebuild UI based on new graph
      // This would involve rendering the UI from the graph structure
      logDebug('Graph updated:', graphData);
    }

    handleValueUpdate(data) {
      const nodeId = data.nodeId;
      const value = data.value;
      const outputName = data.outputName;

      // For plot outputs, value might be a very long base64 string
      const valuePreview = (typeof value === 'string' && value.length > 100)
        ? value.substring(0, 100) + `... [truncated, length=${value.length}]`
        : value;
      logDebug(` Value update received: nodeId=${nodeId}, outputName=${outputName}, value="${valuePreview}"`);

      // Check if this is a plot value
      if (typeof value === 'string' && value.startsWith('data:image/')) {
        logDebug(` Detected plot image data, length: ${value.length}`);
      }

      // If we have an output_name, update that element directly
      if (outputName) {
        const updated = this.updateOutputElement(outputName, value);
        if (updated) {
          logDebug(` Updated output element "${outputName}" with value "${value}"`);
        } else {
          logWarn(` Could not find output element "${outputName}"`);
        }
      } else if (nodeId) {
        // Find and update the corresponding UI element
        // Check if it's an output node
        const node = this.findNodeInGraph(nodeId);
        if (node && node.output_name) {
          this.updateOutputElement(node.output_name, value);
        }
      }

      // Also update UI elements that depend on this value
      if (nodeId) {
        const dependents = this.reactiveClient.getDependents(nodeId);
        for (let dependentId of dependents) {
          // Trigger re-render of dependent nodes
          this.updateNode(dependentId);
        }
      }
    }

    handleDOMPatch(patch) {
      // Apply DOM patch
      if (this.domPatcher) {
        this.domPatcher.patch([patch]);
      }
    }

    handleHotReload(reloadData) {
      logDebug('Hot reload:', reloadData.summary);

      // Notify before reload
      this.notifyHotReload(reloadData);

      // DO NOT reload the page - this would reset input values to defaults!
      // The server has already:
      // 1. Re-executed the server function with preserved input values
      // 2. Sent output value updates via WebSocket
      // So we just need to let those updates be applied to the DOM

      // If the UI structure changed significantly, we may need to fetch new HTML
      // But for now, just log and let value updates handle it
      logDebug(' Hot reload complete - output values will be updated via WebSocket');

      // Show a brief notification to the user
      this.showHotReloadNotification();
    }

    showHotReloadNotification() {
      // Create a temporary notification element
      const notification = document.createElement('div');
      notification.style.cssText = `
        position: fixed;
        top: 10px;
        right: 10px;
        background: #28a745;
        color: white;
        padding: 10px 20px;
        border-radius: 5px;
        z-index: 10000;
        font-family: sans-serif;
        font-size: 14px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.2);
        transition: opacity 0.3s ease;
      `;
      notification.textContent = '✓ Hot reload applied';
      document.body.appendChild(notification);

      // Remove after 2 seconds
      setTimeout(() => {
        notification.style.opacity = '0';
        setTimeout(() => notification.remove(), 300);
      }, 2000);
    }

    updateNode(nodeId) {
      // Get node value and update corresponding UI element
      const value = this.reactiveClient.getValue(nodeId);
      const node = this.findNodeInGraph(nodeId);

      if (node && node.type === 'output') {
        this.updateOutputElement(node.output_name, value);
      }
    }

    updateOutputElement(outputName, value) {
      // Try multiple ways to find the element
      let element = document.getElementById(outputName);
      if (!element) {
        // Try data attribute
        element = document.querySelector(`[data-output-id="${outputName}"]`);
      }
      if (!element) {
        // Try class selector with ID (text output)
        element = document.querySelector(`.shiny-text-output#${outputName}`);
      }
      if (!element) {
        // Try class selector with ID (plot output)
        element = document.querySelector(`.shiny-plot-output#${outputName}`);
      }
      if (!element) {
        // Try just class selector (text output)
        element = document.querySelector(`.shiny-text-output[id="${outputName}"]`);
      }
      if (!element) {
        // Try just class selector (plot output)
        element = document.querySelector(`.shiny-plot-output[id="${outputName}"]`);
      }

      if (element) {
        // Update element based on render type
        const stringValue = value !== null && value !== undefined ? String(value) : '';

        // Check if this is a plot output (has class shiny-plot-output)
        if (element.classList.contains('shiny-plot-output')) {
          logDebug(` Updating plot output element "${outputName}"`);
          logDebug(` Element found:`, element);
          logDebug(` Value type: ${typeof value}, length: ${stringValue.length}`);

          // For plot outputs, check if value is a base64 image
          if (stringValue && stringValue.startsWith('data:image/')) {
            logDebug(` Plot image data detected, length: ${stringValue.length}`);
            logDebug(` Image data preview: ${stringValue.substring(0, 50)}...`);

            // Create or update img element
            let img = element.querySelector('img');
            if (!img) {
              logDebug(` Creating new img element for plot`);
              // Clear any leftover placeholder/error text and styling from an
              // earlier empty/ERROR state (e.g. the transient "argument of
              // length 0" the server emits on first render, before the client
              // has reported its initial input values). Without this the stale
              // text node lingers *above* the freshly-rendered plot until a
              // full page refresh rebuilds the DOM.
              element.textContent = '';
              element.style.color = '';
              element.style.padding = '';
              img = document.createElement('img');
              img.style.maxWidth = '100%';
              img.style.height = 'auto';
              img.style.display = 'block';
              element.appendChild(img);
            }

            // Set the image source
            logDebug(` Setting img.src (length: ${stringValue.length})`);
            img.src = stringValue;

            img.onload = () => {
              logDebug(` Plot image loaded successfully, dimensions: ${img.naturalWidth}x${img.naturalHeight}`);
            };
            img.onerror = (e) => {
              console.error(`[hotShiny] Error loading plot image:`, e);
              console.error(`[hotShiny] Image src length: ${img.src.length}`);
              console.error(`[hotShiny] Image src preview: ${img.src.substring(0, 100)}`);
            };

            logDebug(` Plot img element:`, img);
            return true;
          } else if (stringValue === '' || stringValue === 'null' || stringValue === 'undefined') {
            // No value yet: leave the plot area blank rather than printing a
            // red "not rendered" message. An output that hasn't computed yet
            // (e.g. on first load before inputs arrive) should simply be empty,
            // matching Shiny; the real image replaces this once it is ready.
            logDebug(` Clearing plot output (empty value)`);
            element.textContent = '';
            element.style.color = '';
            element.style.padding = '';
            return true;
          } else if (stringValue.startsWith('ERROR:')) {
            // Show error message from server
            console.error(`[hotShiny] Plot rendering error:`, stringValue);
            element.textContent = stringValue;
            element.style.color = 'red';
            element.style.padding = '10px';
            return true;
          } else {
            logWarn(` Plot output received non-image value:`, stringValue.substring(0, 200));
            logWarn(` Value starts with:`, stringValue.substring(0, 20));
            element.textContent = `Plot error: ${stringValue.substring(0, 100)}`;
            element.style.color = 'red';
            element.style.padding = '10px';
          }
        }

        // For text outputs or other types
        if (element.classList.contains('shiny-html-output')) {
          element.innerHTML = stringValue;
        } else if (element.classList.contains('shiny-text-output')) {
          element.textContent = stringValue;
        } else if (element.tagName === 'DIV' || element.tagName === 'SPAN') {
          element.textContent = stringValue;
        } else {
          element.innerHTML = stringValue;
        }
        return true;
      } else {
        logWarn(` Output element not found: ${outputName}. Available elements:`,
          Array.from(document.querySelectorAll('[id], [data-output-id]')).map(el => ({
            id: el.id,
            dataOutputId: el.getAttribute('data-output-id'),
            tagName: el.tagName
          }))
        );
        return false;
      }
    }

    findNodeInGraph(nodeId) {
      if (!this.reactiveClient.graph || !this.reactiveClient.graph.nodes) {
        return null;
      }

      const nodes = this.reactiveClient.graph.nodes;
      // Handle both array and object formats
      if (Array.isArray(nodes)) {
        return nodes.find(n => n.id === nodeId);
      } else if (typeof nodes === 'object') {
        // If nodes is an object, check if nodeId is a key
        if (nodes[nodeId]) {
          return nodes[nodeId];
        }
        // Otherwise, search through values
        return Object.values(nodes).find(n => n && n.id === nodeId);
      }
      return null;
    }

    notifyHotReload(reloadData) {
      // Dispatch custom event
      const event = new CustomEvent('hotshiny:reload', {
        detail: reloadData
      });
      document.dispatchEvent(event);
    }

    sendInput(inputName, value) {
      if (this.wsClient && this.wsClient.connected) {
        logDebug(` Sending input: ${inputName} = "${value}"`);
        this.wsClient.send('user_input', {
          input_name: inputName,
          value: value
        });
      } else {
        logWarn(' WebSocket not connected, cannot send input');
      }
    }

    getWebSocketUrl() {
      const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
      const host = window.location.hostname || window.location.host.split(':')[0] || 'localhost';
      const port = window.location.port || (window.location.protocol === 'https:' ? '443' : '80');
      // If port is in host, use it; otherwise construct URL
      if (window.location.host.includes(':')) {
        return `${protocol}//${window.location.host}/websocket`;
      } else {
        return `${protocol}//${host}:${port}/websocket`;
      }
    }

    disconnect() {
      if (this.wsClient) {
        this.wsClient.disconnect();
      }
      this.initialized = false;
    }
  }

  // Create global instance
  global.HotShiny = HotShiny;

  // Auto-initialize if DOM is ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => {
      global.hotShiny = new HotShiny();
    });
  } else {
    global.hotShiny = new HotShiny();
  }

})(typeof window !== 'undefined' ? window : this);
