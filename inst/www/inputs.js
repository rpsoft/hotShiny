/**
 * hotShiny Input Bindings
 * Client-side handlers for all input types
 */

(function (global) {
  'use strict';

  // Debug logging - uses global flag from hotshiny.js
  const DEBUG = () => window.HOTSHINY_DEBUG === true;
  const logDebug = (...args) => { if (DEBUG()) console.log(...args); };
  const logWarn = (...args) => { if (DEBUG()) console.warn(...args); };

  // Interpret a value as a boolean. The server stores all scalar inputs as
  // strings (see coerce_input_value in core-values.R), so a checkbox value
  // arrives back as the string "FALSE"/"TRUE" -- and `!!"FALSE"` is true.
  // Mirror the server's coercion here so checkboxes restore correctly.
  function toBool(value) {
    if (typeof value === 'boolean') return value;
    if (typeof value === 'number') return value !== 0;
    if (typeof value === 'string') {
      const v = value.trim().toUpperCase();
      return v === 'TRUE' || v === '1' || v === 'YES' || v === 'ON';
    }
    return !!value;
  }

  // ========================================================================
  // In-place DOM morphing (used by hot reload to avoid full-app flashes)
  // ========================================================================

  // True for elements whose *contents* are owned by something other than the
  // regenerated UI HTML, so morphing must leave their children untouched:
  //   - shiny outputs (filled by separate WebSocket value updates)
  //   - dynamically-inserted UI (insertUI / modals)
  function hasManagedContents(el) {
    if (el.nodeType !== Node.ELEMENT_NODE) return false;
    if (el.hasAttribute('data-output-id')) return true;
    const cls = el.classList;
    return cls.contains('shiny-text-output') ||
           cls.contains('shiny-html-output') ||
           cls.contains('shiny-plot-output') ||
           cls.contains('shiny-image-output') ||
           cls.contains('shiny-table-output');
  }

  // True for live form controls whose user-edited value lives on the DOM
  // property, not the attribute. We must not clobber these from new HTML.
  function isFormControl(el) {
    const t = el.tagName;
    return t === 'INPUT' || t === 'SELECT' || t === 'TEXTAREA' || t === 'OPTION';
  }

  // Attributes that are owned by the client at runtime and must never be
  // overwritten or removed by the regenerated server HTML.
  function isClientOwnedAttr(name, live) {
    // Binding marker: removing it would make initialize() re-subscribe an
    // already-bound node, stacking duplicate change listeners every reload.
    if (name === 'data-hotshiny-bound') return true;
    // User-entered values live on the DOM property of form controls.
    if (live && (name === 'value' || name === 'checked' || name === 'selected')) return true;
    return false;
  }

  // Copy attributes from newEl onto oldEl in place (add/update/remove).
  function syncAttributes(oldEl, newEl) {
    const live = isFormControl(oldEl);
    // Add / update
    for (const attr of newEl.attributes) {
      if (isClientOwnedAttr(attr.name, live)) continue;
      if (oldEl.getAttribute(attr.name) !== attr.value) {
        oldEl.setAttribute(attr.name, attr.value);
      }
    }
    // Remove attributes no longer present
    for (const attr of Array.from(oldEl.attributes)) {
      if (isClientOwnedAttr(attr.name, live)) continue;
      if (!newEl.hasAttribute(attr.name)) {
        oldEl.removeAttribute(attr.name);
      }
    }
  }

  // Decide whether two nodes are "the same node" for morphing purposes.
  function isSameNode(a, b) {
    if (a.nodeType !== b.nodeType) return false;
    if (a.nodeType === Node.ELEMENT_NODE) {
      if (a.id || b.id) return a.id === b.id;
      return a.tagName === b.tagName;
    }
    return true; // text / comment nodes compared by content later
  }

  // Morph the children of oldParent to match newParent, in place.
  function morphChildren(oldParent, newParent) {
    let oldChild = oldParent.firstChild;
    let newChild = newParent.firstChild;

    while (newChild) {
      const nextNew = newChild.nextSibling;

      if (!oldChild) {
        // Nothing left to match against: append a clone of the new node.
        oldParent.appendChild(newChild.cloneNode(true));
        newChild = nextNew;
        continue;
      }

      // Try to find a matching old node at or after the cursor (keyed by id).
      let match = null;
      if (newChild.nodeType === Node.ELEMENT_NODE && newChild.id) {
        let scan = oldChild;
        while (scan) {
          if (scan.nodeType === Node.ELEMENT_NODE && scan.id === newChild.id) { match = scan; break; }
          scan = scan.nextSibling;
        }
      }
      if (!match && isSameNode(oldChild, newChild)) {
        match = oldChild;
      }

      if (match) {
        // Move the matched node into position if needed, then morph it.
        if (match !== oldChild) oldParent.insertBefore(match, oldChild);
        morphNode(match, newChild);
        oldChild = match.nextSibling;
      } else {
        // No match: insert a clone before the current old node.
        oldParent.insertBefore(newChild.cloneNode(true), oldChild);
      }
      newChild = nextNew;
    }

    // Remove any trailing old nodes that weren't matched.
    while (oldChild) {
      const next = oldChild.nextSibling;
      oldParent.removeChild(oldChild);
      oldChild = next;
    }
  }

  // Morph a single node (oldNode) to look like newNode, in place.
  function morphNode(oldNode, newNode) {
    if (oldNode.nodeType === Node.TEXT_NODE || oldNode.nodeType === Node.COMMENT_NODE) {
      if (oldNode.nodeValue !== newNode.nodeValue) oldNode.nodeValue = newNode.nodeValue;
      return;
    }

    if (oldNode.nodeType !== Node.ELEMENT_NODE) return;

    // Leave <script> nodes untouched: a cloned/replaced script inserted via the
    // DOM does not re-execute, so morphing one would silently break it. Scripts
    // that must run on reload are handled via the head-append path instead.
    if (oldNode.tagName === 'SCRIPT' && newNode.tagName === 'SCRIPT') return;

    // Different tag => replace wholesale (can't morph an <a> into a <div>).
    if (oldNode.tagName !== newNode.tagName) {
      oldNode.parentNode.replaceChild(newNode.cloneNode(true), oldNode);
      return;
    }

    syncAttributes(oldNode, newNode);

    // Leave server-rendered / dynamically-managed contents alone.
    if (hasManagedContents(oldNode)) return;

    morphChildren(oldNode, newNode);
  }

  // Snapshot the *live* state (DOM properties, not attributes) of every form
  // control with an id. Needed because a control's user-edited value lives on
  // the property; if morphing has to recreate a node (e.g. an ancestor without
  // an id shifts and gets cloned), the clone resets to its HTML default
  // (a `checked` checkbox snaps back to true). We re-apply this afterwards.
  function snapshotFormState(root) {
    const state = new Map();
    const controls = root.querySelectorAll('input[id], select[id], textarea[id]');
    for (const el of controls) {
      state.set(el.id, {
        type: (el.type || '').toLowerCase(),
        checked: el.checked,
        value: el.value
      });
    }
    return state;
  }

  // Re-apply a snapshot to the morphed DOM, keyed by id. Only ids that still
  // exist are touched, so removed inputs are left out and brand-new inputs
  // keep their server-provided defaults.
  function restoreFormState(root, state) {
    for (const [id, s] of state) {
      const el = root.querySelector(`#${CSS.escape(id)}`);
      if (!el) continue;
      if (s.type === 'checkbox' || s.type === 'radio') {
        if (el.checked !== s.checked) el.checked = s.checked;
      } else if (el.value !== s.value) {
        el.value = s.value;
      }
    }
  }

  // Public entry point: morph `target`'s contents to match `htmlString`.
  // Returns true on success, false if it bailed (caller should fall back).
  function morphInnerHTML(target, htmlString) {
    try {
      const tpl = document.createElement('template');
      tpl.innerHTML = htmlString;
      const formState = snapshotFormState(target);
      morphChildren(target, tpl.content);
      restoreFormState(target, formState);
      return true;
    } catch (e) {
      logWarn('[morph] failed, falling back to innerHTML:', e);
      return false;
    }
  }

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
      // Find single checkboxes (not in checkbox groups)
      // Checkbox groups are handled by CheckboxGroupInputBinding
      // Single checkboxes are in .form-check but not in .shiny-input-checkboxgroup
      return scope.querySelectorAll('.form-check:not(.shiny-input-checkboxgroup) input[type="checkbox"][data-input-id]');
    }

    getValue(el) {
      return el.checked;
    }

    setValue(el, value) {
      el.checked = toBool(value);
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

    setValue(el, value) {
      if (value === null || value === undefined || value === '') return;
      const tabId = el.id;
      const targetLink = el.querySelector(`[data-bs-target="#${tabId}-${value}"]`);
      if (targetLink && global.bootstrap) {
        const tab = new bootstrap.Tab(targetLink);
        tab.show();
      }
    }

    subscribe(el, callback) {
      el.addEventListener('shown.bs.tab', callback);
    }

    receiveMessage(el, message) {
      if (message.selected) {
        this.setValue(el, message.selected);
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
              // Re-read value to ensure we don't overwrite restored state
              // which might have happened during the timeout
              const currentValue = binding.getValue(el);
              this.sendValue(id, currentValue);
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

    /**
     * Capture all current input values for state preservation during hot-reload.
     * This captures values from all registered input bindings in the given scope.
     * @param {Element} scope - The DOM element scope to capture inputs from
     * @returns {Map} A map of inputId -> { value, bindingName }
     */
    captureAllInputState(scope = document) {
      const state = new Map();

      for (const [name, binding] of this.bindings) {
        try {
          const elements = binding.find(scope);
          for (const el of elements) {
            const id = binding.getId(el);
            if (!id) continue;

            const value = binding.getValue(el);
            if (value !== null && value !== undefined) {
              state.set(id, { value, bindingName: name });
              logDebug(`[InputManager] Captured state: ${id} = `, value);
            }
          }
        } catch (e) {
          logWarn(`[InputManager] Error capturing state for binding ${name}:`, e);
        }
      }

      logDebug(`[InputManager] Captured ${state.size} input values for hot-reload`);
      return state;
    }

    /**
     * Restore previously captured input values after DOM replacement.
     * This finds elements by their input ID and restores values using the appropriate binding.
     * @param {Map} state - The captured state from captureAllInputState()
     * @param {Element} scope - The DOM element scope to restore inputs in
     * @param {boolean} sendToServer - Whether to re-send restored values to the server
     */
    restoreInputState(state, scope = document, sendToServer = true) {
      let restored = 0;

      for (const [inputId, data] of state) {
        const binding = this.bindings.get(data.bindingName);
        if (!binding) {
          logWarn(`[InputManager] Binding not found for restore: ${data.bindingName}`);
          continue;
        }

        try {
          // Find the element again in the new DOM
          const elements = binding.find(scope);
          for (const el of elements) {
            const id = binding.getId(el);
            if (id !== inputId) continue;

            // Restore the value
            binding.setValue(el, data.value);
            logDebug(`[InputManager] Restored state: ${inputId} = `, data.value);
            restored++;

            // Update our internal reference
            this.elements.set(id, { element: el, binding: binding });

            // Re-send to server to maintain sync
            if (sendToServer) {
              this.sendValue(inputId, data.value);
            }

            break; // Found and restored, move to next input
          }
        } catch (e) {
          logWarn(`[InputManager] Error restoring state for ${inputId}:`, e);
        }
      }

      logDebug(`[InputManager] Restored ${restored}/${state.size} input values after hot-reload`);
      return restored;
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

    // Replace UI handler (for hot reload UI updates)
    // This handler preserves input state across UI replacements
    global.hotShiny.wsClient.registerHandler('shiny-replace-ui', (message) => {
      const data = message.data;
      const selector = data.selector || '#app';
      const html = data.html || '';

      const target = document.querySelector(selector);
      if (target) {
        // Preferred path: morph the existing DOM toward the new HTML in place.
        // Only nodes that actually differ are touched, so unchanged parts of
        // the app don't repaint -- no full-app flash -- and live input values,
        // focus, scroll position and server-rendered outputs are preserved.
        const morphed = morphInnerHTML(target, html);

        if (!morphed) {
          // Fallback: wholesale replace (causes a flash, but always correct).
          // Capture/restore input state since the nodes are recreated.
          let capturedState = null;
          if (global.hotShinyInputManager) {
            capturedState = global.hotShinyInputManager.captureAllInputState(target);
          }
          target.innerHTML = html;
          if (global.hotShinyInputManager) {
            global.hotShinyInputManager.initialize(document);
          }
          if (capturedState && capturedState.size > 0 && global.hotShinyInputManager) {
            setTimeout(() => {
              global.hotShinyInputManager.restoreInputState(capturedState, target, true);
            }, 50);
          }
        } else {
          // Re-initialize bindings so any newly-added inputs are wired up.
          // Existing nodes were preserved, so their values stay intact.
          if (global.hotShinyInputManager) {
            global.hotShinyInputManager.initialize(document);
          }
          logDebug('[shiny-replace-ui] Morphed UI in place (no flash)');
        }
      } else {
        console.warn('shiny-replace-ui: Target element not found:', selector);
      }
    });

    // Append head content handler (for hot reload that introduces new
    // dependencies / <head> content). Idempotent: a fragment already present in
    // <head> (matched by src/href/exact markup) is skipped, so re-sending the
    // full head on every reload is harmless.
    global.hotShiny.wsClient.registerHandler('shiny-head-append', (message) => {
      const html = (message.data && message.data.html) || '';
      if (!html.trim()) return;

      const tpl = document.createElement('template');
      tpl.innerHTML = html;

      for (const node of Array.from(tpl.content.childNodes)) {
        if (node.nodeType !== Node.ELEMENT_NODE) continue;

        // Dedupe: scripts by src, stylesheets by href, else by exact markup.
        let exists = false;
        if (node.tagName === 'SCRIPT' && node.src) {
          exists = !!document.head.querySelector(`script[src="${CSS.escape(node.getAttribute('src'))}"]`);
        } else if (node.tagName === 'LINK' && node.getAttribute('href')) {
          exists = !!document.head.querySelector(`link[href="${CSS.escape(node.getAttribute('href'))}"]`);
        } else {
          exists = Array.from(document.head.children).some(c => c.outerHTML === node.outerHTML);
        }
        if (exists) continue;

        // <script> inserted via innerHTML/clone won't execute; rebuild it so it
        // runs (e.g. a newly-added Tailwind CDN script).
        if (node.tagName === 'SCRIPT') {
          const s = document.createElement('script');
          for (const attr of node.attributes) s.setAttribute(attr.name, attr.value);
          s.textContent = node.textContent;
          document.head.appendChild(s);
        } else {
          document.head.appendChild(node.cloneNode(true));
        }
      }
      logDebug('[shiny-head-append] Applied head content update');
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

    // Restore inputs handler (from server)
    global.hotShiny.wsClient.registerHandler('shiny-restore-inputs', (message) => {
      const data = message.data;
      const inputManager = global.hotShinyInputManager;

      if (inputManager && data.inputs) {
        logDebug('[shiny-restore-inputs] Restoring inputs from server:', data.inputs);
        let restored = 0;

        // The server sends inputs as { "id": value, ... }
        // We need to ensure we map these to the correct elements

        // Ensure input manager is initialized to find elements
        if (inputManager.elements.size === 0) {
          inputManager.initialize(document);
        }

        for (const [inputId, value] of Object.entries(data.inputs)) {
          // Find binding info for this input ID
          const info = inputManager.elements.get(inputId);

          if (info && info.binding && info.element) {
            try {
              // Only update if value is different? 
              // Usually the server sends preserved values which might be same as default
              // or might be different. Safest is to just set it.
              info.binding.setValue(info.element, value);

              // Also update last value to prevent echo
              info.element.setAttribute('data-hotshiny-last-value', value);

              logDebug(`[shiny-restore-inputs] Restored ${inputId} = ${value}`);
              restored++;
            } catch (e) {
              console.warn(`[shiny-restore-inputs] Error restoring ${inputId}:`, e);
            }
          } else {
            // Element not found in manager. Might be dynamic UI or not yet initialized?
            // Try to find it manually for standard inputs?
            // For now, just log warning
            // logDebug(`[shiny-restore-inputs] Could not find element/binding for ${inputId}`);
          }
        }

        logDebug(`[shiny-restore-inputs] Restored ${restored} inputs from server`);
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
