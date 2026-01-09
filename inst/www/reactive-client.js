/**
 * Reactive Client
 * Maintains reactive graph mirror and handles updates
 */

class ReactiveClient {
  constructor() {
    this.graph = null;
    this.values = new Map();
    this.subscribers = new Map();
  }
  
  updateGraph(graphData) {
    this.graph = graphData;
    this.notifySubscribers('graph_update', graphData);
  }
  
  updateValue(nodeId, value) {
    this.values.set(nodeId, value);
    this.notifySubscribers('value_update', { nodeId, value });
  }
  
  getValue(nodeId) {
    return this.values.get(nodeId);
  }
  
  subscribe(nodeId, callback) {
    if (!this.subscribers.has(nodeId)) {
      this.subscribers.set(nodeId, []);
    }
    this.subscribers.get(nodeId).push(callback);
  }
  
  unsubscribe(nodeId, callback) {
    if (this.subscribers.has(nodeId)) {
      const callbacks = this.subscribers.get(nodeId);
      const index = callbacks.indexOf(callback);
      if (index > -1) {
        callbacks.splice(index, 1);
      }
    }
  }
  
  notifySubscribers(event, data) {
    // Notify all subscribers for the event
    for (let [nodeId, callbacks] of this.subscribers) {
      for (let callback of callbacks) {
        try {
          callback(event, data);
        } catch (error) {
          console.error('Error in subscriber callback:', error);
        }
      }
    }
  }
  
  getDependencies(nodeId) {
    if (!this.graph || !this.graph.nodes) {
      return [];
    }
    
    const nodes = this.graph.nodes;
    let node = null;
    if (Array.isArray(nodes)) {
      node = nodes.find(n => n.id === nodeId);
    } else if (typeof nodes === 'object') {
      node = nodes[nodeId] || Object.values(nodes).find(n => n && n.id === nodeId);
    }
    return node ? (node.deps || []) : [];
  }
  
  getDependents(nodeId) {
    if (!this.graph || !this.graph.edges) {
      return [];
    }
    
    return this.graph.edges
      .filter(edge => edge.from === nodeId)
      .map(edge => edge.to);
  }
}

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
  module.exports = ReactiveClient;
}
