/**
 * Shiny JS Compatibility Shim
 * ---------------------------
 * Exposes a browser-side `Shiny` object on top of hotShiny's WebSocket client,
 * so that ecosystem JavaScript written against Shiny (shinyjs, htmlwidgets,
 * custom message handlers, custom input/output bindings) works under hotShiny
 * without modification.
 *
 * Implemented surface:
 *   Shiny.setInputValue(name, value, opts)   (alias: Shiny.onInputChange)
 *   Shiny.addCustomMessageHandler(type, fn)
 *   Shiny.inputBindings / Shiny.outputBindings  (.register / .setPriority)
 *   Shiny.bindAll(scope) / Shiny.unbindAll(scope)
 *   Shiny.renderDependencies(deps)  (best-effort head injection)
 *   Events: shiny:connected, shiny:sessioninitialized, shiny:bound
 */
(function (global) {
  'use strict';

  var DEBUG = function () { return global.HOTSHINY_DEBUG === true; };
  function log() { if (DEBUG()) console.log.apply(console, ['[Shiny-compat]'].concat([].slice.call(arguments))); }

  // Resolve the hotShiny WebSocket client, retrying until it is ready.
  function withWsClient(cb) {
    var attempts = 0;
    (function tryGet() {
      var hs = global.hotShiny;
      if (hs && hs.wsClient) { cb(hs.wsClient); return; }
      if (attempts++ > 200) { log('wsClient never became available'); return; }
      setTimeout(tryGet, 25);
    })();
  }

  // A registry of custom input/output bindings registered before the client
  // is ready, plus a simple binding collection mirroring Shiny's API.
  function BindingRegistry(kind) {
    this.kind = kind;
    this.bindings = [];
  }
  BindingRegistry.prototype.register = function (binding, name, priority) {
    this.bindings.push({ binding: binding, name: name, priority: priority || 0 });
    // Forward custom INPUT bindings to hotShiny's InputManager when possible so
    // their values flow over the same channel as built-in inputs.
    if (this.kind === 'input' && global.hotShiny && global.hotShiny.inputManager &&
        typeof global.hotShiny.inputManager.registerBinding === 'function') {
      try { global.hotShiny.inputManager.registerBinding(name, binding); } catch (e) { log(e); }
    }
    log('registered ' + this.kind + ' binding: ' + name);
  };
  BindingRegistry.prototype.setPriority = function () { /* no-op */ };
  BindingRegistry.prototype.getBindings = function () {
    return this.bindings.slice().sort(function (a, b) { return b.priority - a.priority; });
  };

  var Shiny = global.Shiny || {};

  Shiny.version = (global.hotShiny && global.hotShiny.version) || 'hotShiny-compat';
  Shiny.inputBindings = new BindingRegistry('input');
  Shiny.outputBindings = new BindingRegistry('output');

  // --- Input values ---------------------------------------------------------
  Shiny.setInputValue = function (name, value, opts) {
    withWsClient(function (ws) {
      if (ws.connected) {
        ws.send('user_input', { input_name: name, value: value });
      } else {
        // Queue until connected.
        var i = setInterval(function () {
          if (ws.connected) {
            clearInterval(i);
            ws.send('user_input', { input_name: name, value: value });
          }
        }, 50);
      }
    });
  };
  Shiny.onInputChange = Shiny.setInputValue;

  // --- Custom message handlers ---------------------------------------------
  Shiny._customMessageHandlers = {};
  Shiny.addCustomMessageHandler = function (type, handler) {
    Shiny._customMessageHandlers[type] = handler;
    withWsClient(function (ws) {
      ws.registerHandler(type, function (message) {
        // Shiny handlers receive the message payload, not the envelope.
        handler(message && message.data !== undefined ? message.data : message);
      });
    });
    log('addCustomMessageHandler: ' + type);
  };

  // --- Binding lifecycle ----------------------------------------------------
  function fireEvent(el, name, detail) {
    try {
      var ev = new CustomEvent(name, { bubbles: true, detail: detail || {} });
      (el || document).dispatchEvent(ev);
    } catch (e) { /* older browsers */ }
  }

  Shiny.bindAll = function (scope) {
    scope = scope || document;
    // Let hotShiny's own input manager bind built-in inputs in the new subtree.
    if (global.hotShiny && global.hotShiny.inputManager &&
        typeof global.hotShiny.inputManager.bindAll === 'function') {
      try { global.hotShiny.inputManager.bindAll(scope); } catch (e) { log(e); }
    }
    // Bind registered custom bindings.
    Shiny.inputBindings.getBindings().forEach(function (b) {
      if (typeof b.binding.find !== 'function') return;
      var els = b.binding.find(scope) || [];
      Array.prototype.forEach.call(els, function (el) {
        try {
          if (typeof b.binding.initialize === 'function') b.binding.initialize(el);
          if (typeof b.binding.subscribe === 'function') {
            b.binding.subscribe(el, function () {
              var id = b.binding.getId(el);
              var val = b.binding.getValue(el);
              Shiny.setInputValue(id, val);
            });
          }
          fireEvent(el, 'shiny:bound', { binding: b.binding, bindingType: 'input' });
        } catch (e) { log(e); }
      });
    });
  };

  Shiny.unbindAll = function (scope) {
    scope = scope || document;
    if (global.hotShiny && global.hotShiny.inputManager &&
        typeof global.hotShiny.inputManager.unbindAll === 'function') {
      try { global.hotShiny.inputManager.unbindAll(scope); } catch (e) { log(e); }
    }
  };

  // --- htmlDependency injection (best-effort) -------------------------------
  Shiny.renderDependencies = function (deps) {
    if (!deps) return;
    deps.forEach(function (dep) {
      var base = '/' + (dep.name || '');
      (dep.stylesheet ? [].concat(dep.stylesheet) : []).forEach(function (css) {
        if (document.querySelector('link[data-hs-dep="' + dep.name + '/' + css + '"]')) return;
        var link = document.createElement('link');
        link.rel = 'stylesheet';
        link.href = base + '/' + css;
        link.setAttribute('data-hs-dep', dep.name + '/' + css);
        document.head.appendChild(link);
      });
      (dep.script ? [].concat(dep.script) : []).forEach(function (js) {
        if (document.querySelector('script[data-hs-dep="' + dep.name + '/' + js + '"]')) return;
        var s = document.createElement('script');
        s.src = base + '/' + js;
        s.setAttribute('data-hs-dep', dep.name + '/' + js);
        document.head.appendChild(s);
      });
    });
  };
  Shiny.renderContent = function (el, content) {
    if (!el || !content) return;
    if (typeof content === 'string') { el.innerHTML = content; return; }
    if (content.html !== undefined) el.innerHTML = content.html;
    if (content.deps) Shiny.renderDependencies(content.deps);
    Shiny.bindAll(el);
  };

  // Minimal shinyapp object some packages reach for.
  Shiny.shinyapp = Shiny.shinyapp || {
    $sendInput: function (values) {
      Object.keys(values || {}).forEach(function (k) { Shiny.setInputValue(k, values[k]); });
    },
    isConnected: function () { return !!(global.hotShiny && global.hotShiny.wsClient && global.hotShiny.wsClient.connected); }
  };

  global.Shiny = Shiny;

  // Announce connection so ecosystem code waiting on shiny:connected proceeds.
  withWsClient(function (ws) {
    function announce() {
      fireEvent(document, 'shiny:connected');
      fireEvent(document, 'shiny:sessioninitialized');
      Shiny.bindAll(document);
    }
    if (ws.connected) { announce(); }
    else {
      var i = setInterval(function () {
        if (ws.connected) { clearInterval(i); announce(); }
      }, 50);
    }
  });

  log('Shiny compatibility shim installed');
})(typeof window !== 'undefined' ? window : this);
