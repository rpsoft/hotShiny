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

      console.log(`[hotShiny] Value update received: nodeId=${nodeId}, outputName=${outputName}, value="${value}"`);

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
        // Try class selector with ID
        element = document.querySelector(`.shiny-text-output#${outputName}`);
      }
      if (!element) {
        // Try just class selector
        element = document.querySelector(`.shiny-text-output[id="${outputName}"]`);
      }

      if (element) {
        // Update element based on render type
        const stringValue = value !== null && value !== undefined ? String(value) : '';
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
