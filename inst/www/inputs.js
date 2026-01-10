/**
 * hotShiny Input Bindings
 * Client-side handlers for all input types
 */

(function(global) {
  'use strict';
  
  // Debug logging - uses global flag from hotshiny.js
  const DEBUG = () => window.HOTSHINY_DEBUG === true;
  const logDebug = (...args) => { if (DEBUG()) console.log(...args); };
  const logWarn = (...args) => { if (DEBUG()) console.warn(...args); };

  // ========================================================================
  // Input Binding Base Class
  // ========================================================================

  class InputBinding {
    constructor() {
      this.name = 'base';
    }

    find(scope) {
      return [];
    }

    getId(el) {
      return el.getAttribute('data-input-id') || el.id || el.name;
    }

    getValue(el) {
      return el.value;
    }

    setValue(el, value) {
      el.value = value;
    }

    subscribe(el, callback) {
      // Default: listen for input and change events
      el.addEventListener('input', callback);
      el.addEventListener('change', callback);
    }

    unsubscribe(el) {
      // Remove listeners (would need stored references)
    }

    receiveMessage(el, message) {
      if (message.value !== undefined && message.value !== null) {
        this.setValue(el, message.value);
      }
      if (message.label !== undefined && message.label !== null) {
        this.updateLabel(el, message.label);
      }
    }

    updateLabel(el, label) {
      const container = el.closest('.shiny-input-container');
      if (container) {
        const labelEl = container.querySelector('label');
        if (labelEl) {
          labelEl.textContent = label;
        }
      }
    }
  }

  // ========================================================================
  // Text Input Binding
  // ========================================================================

  class TextInputBinding extends InputBinding {
    constructor() {
      super();
      this.name = 'text';
    }

    find(scope) {
      return scope.querySelectorAll('input[type="text"][data-input-id], input[type="text"].form-control');
    }

    receiveMessage(el, message) {
      super.receiveMessage(el, message);
      if (message.placeholder !== undefined) {
        el.placeholder = message.placeholder || '';
      }
    }
  }

  // ========================================================================
  // Textarea Input Binding
  // ========================================================================

  class TextAreaInputBinding extends InputBinding {
    constructor() {
      super();
      this.name = 'textarea';
    }

    find(scope) {
      return scope.querySelectorAll('textarea[data-input-id]');
    }

    receiveMessage(el, message) {
      super.receiveMessage(el, message);
      if (message.placeholder !== undefined) {
        el.placeholder = message.placeholder || '';
      }
    }
  }

  // ========================================================================
  // Password Input Binding
  // ========================================================================

  class PasswordInputBinding extends TextInputBinding {
    constructor() {
      super();
      this.name = 'password';
    }

    find(scope) {
      return scope.querySelectorAll('input[type="password"][data-input-id]');
    }
  }

  // ========================================================================
  // Numeric Input Binding
  // ========================================================================

  class NumericInputBinding extends InputBinding {
    constructor() {
      super();
      this.name = 'numeric';
    }

    find(scope) {
      return scope.querySelectorAll('input[type="number"][data-input-id]');
    }

    getValue(el) {
      const val = el.value;
      if (val === '' || val === null || val === undefined) {
        return null;
      }
      const num = parseFloat(val);
      return isNaN(num) ? null : num;
    }

    receiveMessage(el, message) {
      super.receiveMessage(el, message);
      if (message.min !== undefined) {
        el.min = message.min;
      }
      if (message.max !== undefined) {
        el.max = message.max;
      }
      if (message.step !== undefined) {
        el.step = message.step;
      }
    }
  }

  // ========================================================================
  // Select Input Binding
  // ========================================================================

  class SelectInputBinding extends InputBinding {
    constructor() {
      super();
      this.name = 'select';
    }

    find(scope) {
      return scope.querySelectorAll('select[data-input-id]');
    }

    getValue(el) {
      if (el.multiple) {
        const selected = [];
        for (const option of el.selectedOptions) {
          selected.push(option.value);
        }
        return selected;
      }
      return el.value;
    }

    setValue(el, value) {
      if (el.multiple && Array.isArray(value)) {
        for (const option of el.options) {
          option.selected = value.includes(option.value);
        }
      } else {
        el.value = value;
      }
    }

    receiveMessage(el, message) {
      // Update choices if provided
      if (message.choices) {
        el.innerHTML = '';
        for (const choice of message.choices) {
          const option = document.createElement('option');
          option.value = choice.value;
          option.textContent = choice.label;
          el.appendChild(option);
        }
      }

      // Update selected
      if (message.selected !== undefined && message.selected !== null) {
        this.setValue(el, message.selected);
      }

      super.receiveMessage(el, message);
    }
  }

  // ========================================================================
  // Checkbox Input Binding
  // ========================================================================

  class CheckboxInputBinding extends InputBinding {
    constructor() {
      super();
      this.name = 'checkbox';
    }

    find(scope) {
      return scope.querySelectorAll('.form-check input[type="checkbox"][data-input-id]:not([name])');
    }

    getValue(el) {
      return el.checked;
    }

    setValue(el, value) {
      el.checked = !!value;
    }

    receiveMessage(el, message) {
      if (message.value !== undefined && message.value !== null) {
        this.setValue(el, message.value);
      }
      super.receiveMessage(el, message);
    }
  }

  // ========================================================================
  // Checkbox Group Input Binding
  // ========================================================================

  class CheckboxGroupInputBinding extends InputBinding {
    constructor() {
      super();
      this.name = 'checkboxGroup';
    }

    find(scope) {
      return scope.querySelectorAll('.shiny-input-checkboxgroup[data-input-id]');
    }

    getValue(el) {
      const checkboxes = el.querySelectorAll('input[type="checkbox"]:checked');
      const values = [];
      for (const cb of checkboxes) {
        values.push(cb.value);
      }
      return values;
    }

    setValue(el, values) {
      const checkboxes = el.querySelectorAll('input[type="checkbox"]');
      for (const cb of checkboxes) {
        cb.checked = values.includes(cb.value);
      }
    }

    subscribe(el, callback) {
      el.addEventListener('change', callback);
    }

    receiveMessage(el, message) {
      if (message.selected !== undefined && message.selected !== null) {
        this.setValue(el, Array.isArray(message.selected) ? message.selected : [message.selected]);
      }
      super.receiveMessage(el, message);
    }
  }

  // ========================================================================
  // Radio Buttons Input Binding
  // ========================================================================

  class RadioInputBinding extends InputBinding {
    constructor() {
      super();
      this.name = 'radio';
    }

    find(scope) {
      return scope.querySelectorAll('.shiny-input-radiogroup[data-input-id]');
    }

    getValue(el) {
      const checked = el.querySelector('input[type="radio"]:checked');
      return checked ? checked.value : null;
    }

    setValue(el, value) {
      const radios = el.querySelectorAll('input[type="radio"]');
      for (const radio of radios) {
        radio.checked = (radio.value === value);
      }
    }

    subscribe(el, callback) {
      el.addEventListener('change', callback);
    }

    receiveMessage(el, message) {
      if (message.selected !== undefined && message.selected !== null) {
        this.setValue(el, message.selected);
      }
      super.receiveMessage(el, message);
    }
  }

  // ========================================================================
  // Date Input Binding
  // ========================================================================

  class DateInputBinding extends InputBinding {
    constructor() {
      super();
      this.name = 'date';
    }

    find(scope) {
      return scope.querySelectorAll('input[type="date"][data-input-id]');
    }

    getValue(el) {
      return el.value || null;
    }

    receiveMessage(el, message) {
      super.receiveMessage(el, message);
      if (message.min !== undefined) {
        el.min = message.min;
      }
      if (message.max !== undefined) {
        el.max = message.max;
      }
    }
  }

  // ========================================================================
  // Date Range Input Binding
  // ========================================================================

  class DateRangeInputBinding extends InputBinding {
    constructor() {
      super();
      this.name = 'dateRange';
    }

    find(scope) {
      return scope.querySelectorAll('.shiny-date-range-input[data-input-id]');
    }

    getValue(el) {
      const startInput = el.querySelector('input[id$="_start"]');
      const endInput = el.querySelector('input[id$="_end"]');
      return [
        startInput ? startInput.value : null,
        endInput ? endInput.value : null
      ];
    }

    setValue(el, value) {
      if (Array.isArray(value) && value.length >= 2) {
        const startInput = el.querySelector('input[id$="_start"]');
        const endInput = el.querySelector('input[id$="_end"]');
        if (startInput) startInput.value = value[0] || '';
        if (endInput) endInput.value = value[1] || '';
      }
    }

    subscribe(el, callback) {
      const inputs = el.querySelectorAll('input[type="date"]');
      for (const input of inputs) {
        input.addEventListener('change', callback);
      }
    }

    receiveMessage(el, message) {
      const startInput = el.querySelector('input[id$="_start"]');
      const endInput = el.querySelector('input[id$="_end"]');

      if (message.start !== undefined && startInput) {
        startInput.value = message.start || '';
      }
      if (message.end !== undefined && endInput) {
        endInput.value = message.end || '';
      }
      if (message.min !== undefined) {
        if (startInput) startInput.min = message.min;
        if (endInput) endInput.min = message.min;
      }
      if (message.max !== undefined) {
        if (startInput) startInput.max = message.max;
        if (endInput) endInput.max = message.max;
      }

      super.receiveMessage(el, message);
    }
  }

  // ========================================================================
  // Slider Input Binding
  // ========================================================================

  class SliderInputBinding extends InputBinding {
    constructor() {
      super();
      this.name = 'slider';
    }

    find(scope) {
      return scope.querySelectorAll('input[type="range"][data-input-id]');
    }

    getValue(el) {
      const val = parseFloat(el.value);
      return isNaN(val) ? null : val;
    }

    setValue(el, value) {
      el.value = value;
      this.updateDisplay(el, value);
    }

    subscribe(el, callback) {
      el.addEventListener('input', (e) => {
        this.updateDisplay(el, el.value);
        callback(e);
      });
      el.addEventListener('change', callback);
    }

    updateDisplay(el, value) {
      const outputId = el.id + '_value';
      const output = document.getElementById(outputId);
      if (output) {
        const pre = el.getAttribute('data-pre') || '';
        const post = el.getAttribute('data-post') || '';
        output.textContent = pre + value + post;
      }
    }

    receiveMessage(el, message) {
      if (message.min !== undefined) {
        el.min = message.min;
      }
      if (message.max !== undefined) {
        el.max = message.max;
      }
      if (message.step !== undefined) {
        el.step = message.step;
      }
      if (message.value !== undefined && message.value !== null) {
        this.setValue(el, message.value);
      }
      super.receiveMessage(el, message);
    }
  }

  // ========================================================================
  // Action Button Binding
  // ========================================================================

  class ActionButtonBinding extends InputBinding {
    constructor() {
      super();
      this.name = 'actionButton';
    }

    find(scope) {
      return scope.querySelectorAll('.action-button[data-input-id]');
    }

    getValue(el) {
      return parseInt(el.getAttribute('data-val') || '0', 10);
    }

    subscribe(el, callback) {
      el.addEventListener('click', (e) => {
        e.preventDefault();
        const currentVal = this.getValue(el);
        el.setAttribute('data-val', currentVal + 1);
        callback(e);
      });
    }

    receiveMessage(el, message) {
      if (message.label !== undefined && message.label !== null) {
        // Preserve icon if present
        const icon = el.querySelector('i, span.fa');
        el.innerHTML = '';
        if (icon) {
          el.appendChild(icon);
          el.appendChild(document.createTextNode(' '));
        }
        el.appendChild(document.createTextNode(message.label));
      }
      if (message.icon !== undefined && message.icon !== null) {
        const existingIcon = el.querySelector('i, span.fa');
        if (existingIcon) {
          existingIcon.outerHTML = message.icon;
        } else {
          el.insertAdjacentHTML('afterbegin', message.icon + ' ');
        }
      }
      if (message.disabled !== undefined) {
        el.disabled = message.disabled;
      }
    }
  }

  // ========================================================================
  // File Input Binding
  // ========================================================================

  class FileInputBinding extends InputBinding {
    constructor() {
      super();
      this.name = 'file';
    }

    find(scope) {
      return scope.querySelectorAll('input[type="file"][data-input-id]');
    }

    getValue(el) {
      if (!el.files || el.files.length === 0) {
        return null;
      }
      
      const files = [];
      for (const file of el.files) {
        files.push({
          name: file.name,
          size: file.size,
          type: file.type
        });
      }
      return files;
    }

    subscribe(el, callback) {
      el.addEventListener('change', callback);
    }
  }

  // ========================================================================
  // Tabset Panel Binding
  // ========================================================================

  class TabsetInputBinding extends InputBinding {
    constructor() {
      super();
      this.name = 'tabset';
    }

    find(scope) {
      return scope.querySelectorAll('.nav[data-input-id]');
    }

    getValue(el) {
      const active = el.querySelector('.nav-link.active');
      if (active) {
        const target = active.getAttribute('data-bs-target') || active.getAttribute('href');
        if (target) {
          // Extract value from target ID (e.g., "#tabset_123-tab1" -> "tab1")
          const parts = target.split('-');
          return parts[parts.length - 1];
        }
      }
      return null;
    }

    subscribe(el, callback) {
      el.addEventListener('shown.bs.tab', callback);
    }

    receiveMessage(el, message) {
      if (message.selected) {
        const tabId = el.id;
        const targetLink = el.querySelector(`[data-bs-target="#${tabId}-${message.selected}"]`);
        if (targetLink) {
          const tab = new bootstrap.Tab(targetLink);
          tab.show();
        }
      }
    }
  }

  // ========================================================================
  // Input Manager
  // ========================================================================

  class InputManager {
    constructor() {
      this.bindings = new Map();
      this.elements = new Map();
      
      // Register default bindings
      this.register(new TextInputBinding());
      this.register(new TextAreaInputBinding());
      this.register(new PasswordInputBinding());
      this.register(new NumericInputBinding());
      this.register(new SelectInputBinding());
      this.register(new CheckboxInputBinding());
      this.register(new CheckboxGroupInputBinding());
      this.register(new RadioInputBinding());
      this.register(new DateInputBinding());
      this.register(new DateRangeInputBinding());
      this.register(new SliderInputBinding());
      this.register(new ActionButtonBinding());
      this.register(new FileInputBinding());
      this.register(new TabsetInputBinding());
    }

    register(binding) {
      this.bindings.set(binding.name, binding);
    }

    initialize(scope = document) {
      for (const [name, binding] of this.bindings) {
        const elements = binding.find(scope);
        
        for (const el of elements) {
          const id = binding.getId(el);
          if (!id) continue;

          // Skip if already initialized
          if (el.hasAttribute('data-hotshiny-bound')) continue;
          el.setAttribute('data-hotshiny-bound', name);

          // Store element reference
          this.elements.set(id, { element: el, binding: binding });

          // Subscribe to changes
          binding.subscribe(el, () => {
            const value = binding.getValue(el);
            this.sendValue(id, value);
          });

          // Send initial value
          const initialValue = binding.getValue(el);
          if (initialValue !== null && initialValue !== undefined && initialValue !== '') {
            setTimeout(() => {
              this.sendValue(id, initialValue);
            }, 100);
          }
        }
      }
    }

    sendValue(inputId, value) {
      // Use hotShiny WebSocket client to send value
      if (global.hotShiny && global.hotShiny.wsClient && global.hotShiny.wsClient.connected) {
        logDebug(`[InputManager] Sending: ${inputId} = `, value);
        global.hotShiny.wsClient.send('user_input', {
          input_name: inputId,
          value: value
        });
      } else {
        logWarn(`[InputManager] WebSocket not connected, cannot send ${inputId}`);
      }
    }

    receiveMessage(inputId, message) {
      const info = this.elements.get(inputId);
      if (info) {
        info.binding.receiveMessage(info.element, message);
      }
    }

    getValue(inputId) {
      const info = this.elements.get(inputId);
      if (info) {
        return info.binding.getValue(info.element);
      }
      return null;
    }
  }

  // ========================================================================
  // Custom Message Handlers
  // ========================================================================

  function setupCustomMessageHandlers() {
    if (!global.hotShiny || !global.hotShiny.wsClient) {
      setTimeout(setupCustomMessageHandlers, 100);
      return;
    }

    // Modal handler
    global.hotShiny.wsClient.registerHandler('shiny-modal', (message) => {
      const data = message.data;
      if (data.action === 'show') {
        // Create modal container if not exists
        let container = document.getElementById('shiny-modal-container');
        if (!container) {
          container = document.createElement('div');
          container.id = 'shiny-modal-container';
          document.body.appendChild(container);
        }
        
        container.innerHTML = data.html;
        const modalEl = container.querySelector('.modal');
        if (modalEl && typeof bootstrap !== 'undefined') {
          const modal = new bootstrap.Modal(modalEl);
          modal.show();
        }
      } else if (data.action === 'remove') {
        const modalEl = document.querySelector('.modal.show');
        if (modalEl && typeof bootstrap !== 'undefined') {
          const modal = bootstrap.Modal.getInstance(modalEl);
          if (modal) modal.hide();
        }
      }
    });

    // Notification handler
    global.hotShiny.wsClient.registerHandler('shiny-notification', (message) => {
      const data = message.data;
      
      // Create notification container if not exists
      let container = document.getElementById('shiny-notification-container');
      if (!container) {
        container = document.createElement('div');
        container.id = 'shiny-notification-container';
        container.style.cssText = 'position: fixed; top: 20px; right: 20px; z-index: 1050; width: 350px;';
        document.body.appendChild(container);
      }

      if (data.action === 'show') {
        container.insertAdjacentHTML('beforeend', data.html);
        
        if (data.duration) {
          setTimeout(() => {
            const el = document.getElementById(data.id);
            if (el) el.remove();
          }, data.duration * 1000);
        }
      } else if (data.action === 'remove') {
        const el = document.getElementById(data.id);
        if (el) el.remove();
      }
    });

    // Progress handler
    global.hotShiny.wsClient.registerHandler('shiny-progress', (message) => {
      const data = message.data;
      
      // Create progress container if not exists
      let container = document.getElementById('shiny-progress-container');
      if (!container) {
        container = document.createElement('div');
        container.id = 'shiny-progress-container';
        container.style.cssText = 'position: fixed; top: 0; left: 0; right: 0; z-index: 1060;';
        document.body.appendChild(container);
      }

      if (data.action === 'open') {
        const html = `
          <div id="${data.id}" class="shiny-progress" style="background: #f8f9fa; padding: 10px; border-bottom: 1px solid #dee2e6;">
            <div class="progress-message"></div>
            <div class="progress" style="height: 5px;">
              <div class="progress-bar" role="progressbar" style="width: 0%"></div>
            </div>
            <div class="progress-detail text-muted small"></div>
          </div>
        `;
        container.insertAdjacentHTML('beforeend', html);
      } else if (data.action === 'set' || data.action === 'inc') {
        const el = document.getElementById(data.id) || container.querySelector('.shiny-progress');
        if (el) {
          const bar = el.querySelector('.progress-bar');
          if (bar && data.value !== undefined) {
            bar.style.width = data.value + '%';
          }
          if (data.message !== undefined) {
            const msg = el.querySelector('.progress-message');
            if (msg) msg.textContent = data.message;
          }
          if (data.detail !== undefined) {
            const det = el.querySelector('.progress-detail');
            if (det) det.textContent = data.detail;
          }
        }
      } else if (data.action === 'close') {
        const el = document.getElementById(data.id);
        if (el) el.remove();
      }
    });

    // Insert UI handler
    global.hotShiny.wsClient.registerHandler('shiny-insert-ui', (message) => {
      const data = message.data;
      const targets = data.multiple 
        ? document.querySelectorAll(data.selector)
        : [document.querySelector(data.selector)];

      for (const target of targets) {
        if (!target) continue;
        target.insertAdjacentHTML(data.where, data.html);
      }

      // Re-initialize inputs in new content
      if (global.hotShinyInputManager) {
        global.hotShinyInputManager.initialize(document);
      }
    });

    // Remove UI handler
    global.hotShiny.wsClient.registerHandler('shiny-remove-ui', (message) => {
      const data = message.data;
      const targets = data.multiple
        ? document.querySelectorAll(data.selector)
        : [document.querySelector(data.selector)];

      for (const target of targets) {
        if (target) target.remove();
      }
    });

    // Update query string handler
    global.hotShiny.wsClient.registerHandler('shiny-update-query-string', (message) => {
      const data = message.data;
      const url = new URL(window.location);
      url.search = data.queryString;
      
      if (data.mode === 'push') {
        window.history.pushState({}, '', url);
      } else {
        window.history.replaceState({}, '', url);
      }
    });
  }

  // ========================================================================
  // Initialization
  // ========================================================================

  // Create global input manager
  global.hotShinyInputManager = new InputManager();

  // Initialize when DOM is ready
  function init() {
    global.hotShinyInputManager.initialize(document);
    setupCustomMessageHandlers();

    // Re-initialize on DOM changes
    const observer = new MutationObserver(() => {
      global.hotShinyInputManager.initialize(document);
    });
    observer.observe(document.body, { childList: true, subtree: true });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

  // Export classes for extension
  global.InputBinding = InputBinding;
  global.InputManager = InputManager;

})(typeof window !== 'undefined' ? window : this);
