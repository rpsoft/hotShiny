/**
 * HotShiny Main Client Library
 * Orchestrates client-side functionality
 */

(function (global) {
  'use strict';

  // Import dependencies (would be loaded separately in real implementation)
  // const WebSocketClient = require('./websocket-client.js');
  // const { VirtualDOM, DOMDiff, DOMPatcher } = require('./dom-diff.js');
  // const ReactiveClient = require('./reactive-client.js');

  class HotShiny {
    constructor(options = {}) {
      this.options = {
        wsUrl: options.wsUrl || this.getWebSocketUrl(),
        autoConnect: options.autoConnect !== false,
        ...options
      };

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

        // Send initial value if it exists
        const initialValue = input.value || '';
        if (initialValue !== '') {
          console.log(`[hotShiny] Sending initial value for ${inputId}: ${initialValue}`);
          // Use a small delay to ensure WebSocket is connected
          setTimeout(() => {
            this.sendInput(inputId, initialValue);
          }, 100);
        }

        // Attach input event (fires on every keystroke)
        input.addEventListener('input', (e) => {
          const value = e.target.value || '';
          this.sendInput(inputId, value);
        });

        // Also attach change event (fires on blur/enter)
        input.addEventListener('change', (e) => {
          const value = e.target.value || '';
          this.sendInput(inputId, value);
        });
      });
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
    }

    handleGraphUpdate(graphData) {
      // Rebuild UI based on new graph
      // This would involve rendering the UI from the graph structure
      console.log('Graph updated:', graphData);
    }

    handleValueUpdate(data) {
      const nodeId = data.nodeId;
      const value = data.value;
      const outputName = data.outputName;

      // For plot outputs, value might be a very long base64 string
      const valuePreview = (typeof value === 'string' && value.length > 100) 
        ? value.substring(0, 100) + `... [truncated, length=${value.length}]`
        : value;
      console.log(`[hotShiny] Value update received: nodeId=${nodeId}, outputName=${outputName}, value="${valuePreview}"`);
      
      // Check if this is a plot value
      if (typeof value === 'string' && value.startsWith('data:image/')) {
        console.log(`[hotShiny] Detected plot image data, length: ${value.length}`);
      }

      // If we have an output_name, update that element directly
      if (outputName) {
        const updated = this.updateOutputElement(outputName, value);
        if (updated) {
          console.log(`[hotShiny] Updated output element "${outputName}" with value "${value}"`);
        } else {
          console.warn(`[hotShiny] Could not find output element "${outputName}"`);
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
      console.log('Hot reload:', reloadData.summary);

      // Notify before reload
      this.notifyHotReload(reloadData);

      // Reload the page to get the updated UI
      // Use a small delay to ensure the notification is processed
      setTimeout(() => {
        window.location.reload();
      }, 100);
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
          console.log(`[hotShiny] Updating plot output element "${outputName}"`);
          console.log(`[hotShiny] Element found:`, element);
          console.log(`[hotShiny] Value type: ${typeof value}, length: ${stringValue.length}`);
          
          // For plot outputs, check if value is a base64 image
          if (stringValue && stringValue.startsWith('data:image/')) {
            console.log(`[hotShiny] Plot image data detected, length: ${stringValue.length}`);
            console.log(`[hotShiny] Image data preview: ${stringValue.substring(0, 50)}...`);
            
            // Create or update img element
            let img = element.querySelector('img');
            if (!img) {
              console.log(`[hotShiny] Creating new img element for plot`);
              img = document.createElement('img');
              img.style.maxWidth = '100%';
              img.style.height = 'auto';
              img.style.display = 'block';
              element.appendChild(img);
            }
            
            // Set the image source
            console.log(`[hotShiny] Setting img.src (length: ${stringValue.length})`);
            img.src = stringValue;
            
            img.onload = () => {
              console.log(`[hotShiny] Plot image loaded successfully, dimensions: ${img.naturalWidth}x${img.naturalHeight}`);
            };
            img.onerror = (e) => {
              console.error(`[hotShiny] Error loading plot image:`, e);
              console.error(`[hotShiny] Image src length: ${img.src.length}`);
              console.error(`[hotShiny] Image src preview: ${img.src.substring(0, 100)}`);
            };
            
            console.log(`[hotShiny] Plot img element:`, img);
            return true;
          } else if (stringValue === '' || stringValue === 'null' || stringValue === 'undefined') {
            // Clear the plot
            console.log(`[hotShiny] Clearing plot output (empty value)`);
            const img = element.querySelector('img');
            if (img) {
              img.remove();
            }
            // Show error message if value is empty
            element.textContent = 'Plot not rendered (empty value from server)';
            element.style.color = 'red';
            element.style.padding = '10px';
            return true;
          } else if (stringValue.startsWith('ERROR:')) {
            // Show error message from server
            console.error(`[hotShiny] Plot rendering error:`, stringValue);
            element.textContent = stringValue;
            element.style.color = 'red';
            element.style.padding = '10px';
            return true;
          } else {
            console.warn(`[hotShiny] Plot output received non-image value:`, stringValue.substring(0, 200));
            console.warn(`[hotShiny] Value starts with:`, stringValue.substring(0, 20));
            element.textContent = `Plot error: ${stringValue.substring(0, 100)}`;
            element.style.color = 'red';
            element.style.padding = '10px';
          }
        }
        
        // For text outputs or other types
        if (element.tagName === 'DIV' || element.tagName === 'SPAN') {
          element.textContent = stringValue;
        } else {
          element.innerHTML = stringValue;
        }
        return true;
      } else {
        console.warn(`[hotShiny] Output element not found: ${outputName}. Available elements:`,
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
        console.log(`[hotShiny] Sending input: ${inputName} = "${value}"`);
        this.wsClient.send('user_input', {
          input_name: inputName,
          value: value
        });
      } else {
        console.warn('[hotShiny] WebSocket not connected, cannot send input');
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
