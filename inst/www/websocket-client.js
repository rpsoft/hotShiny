/**
 * WebSocket Client
 * Handles WebSocket communication with server
 */

class WebSocketClient {
  constructor(url) {
    this.url = url;
    this.ws = null;
    this.reconnectAttempts = 0;
    this.maxReconnectAttempts = 10;
    this.reconnectDelay = 1000;
    this.messageHandlers = new Map();
    this.connected = false;
    
    this.connect();
  }
  
  connect() {
    try {
      console.log(`[WebSocket] Connecting to ${this.url}...`);
      this.ws = new WebSocket(this.url);
      
      this.ws.onopen = () => {
        console.log('[WebSocket] Connection opened');
        this.connected = true;
        this.reconnectAttempts = 0;
        this.onOpen();
      };
      
      this.ws.onmessage = (event) => {
        this.handleMessage(event);
      };
      
      this.ws.onerror = (error) => {
        console.error('[WebSocket] Error:', error);
        this.onError(error);
      };
      
      this.ws.onclose = (event) => {
        console.log(`[WebSocket] Connection closed (code: ${event.code}, reason: ${event.reason || 'none'})`);
        this.connected = false;
        this.onClose();
        this.attemptReconnect();
      };
    } catch (error) {
      console.error('[WebSocket] Connection error:', error);
      this.attemptReconnect();
    }
  }
  
  onOpen() {
    console.log('WebSocket connected');
    // Send ping to keep connection alive
    this.startPingInterval();
  }
  
  onClose() {
    console.log('WebSocket disconnected');
    this.stopPingInterval();
  }
  
  onError(error) {
    console.error('WebSocket error:', error);
  }
  
  handleMessage(event) {
    try {
      const message = JSON.parse(event.data);
      console.log(`[WebSocket] Received message type: ${message.type}`, message.data);
      const handler = this.messageHandlers.get(message.type);
      
      if (handler) {
        handler(message);
      } else {
        console.warn(`[WebSocket] No handler for message type: ${message.type}`);
      }
    } catch (error) {
      console.error('[WebSocket] Error handling message:', error, event.data);
    }
  }
  
  send(type, data) {
    if (!this.connected || !this.ws) {
      console.warn('[WebSocket] Not connected, cannot send message:', type);
      return;
    }
    
    const message = {
      type: type,
      data: data,
      timestamp: Date.now()
    };
    
    try {
      const jsonMessage = JSON.stringify(message);
      console.log(`[WebSocket] Sending message type: ${type}`, data);
      this.ws.send(jsonMessage);
    } catch (error) {
      console.error('[WebSocket] Error sending message:', error);
    }
  }
  
  registerHandler(messageType, handler) {
    this.messageHandlers.set(messageType, handler);
  }
  
  attemptReconnect() {
    if (this.reconnectAttempts >= this.maxReconnectAttempts) {
      console.error('Max reconnection attempts reached');
      return;
    }
    
    this.reconnectAttempts++;
    const delay = this.reconnectDelay * Math.pow(2, this.reconnectAttempts - 1);
    
    console.log(`Attempting to reconnect in ${delay}ms (attempt ${this.reconnectAttempts})`);
    
    setTimeout(() => {
      this.connect();
    }, delay);
  }
  
  startPingInterval() {
    this.pingInterval = setInterval(() => {
      this.send('ping', {});
    }, 30000); // Ping every 30 seconds
  }
  
  stopPingInterval() {
    if (this.pingInterval) {
      clearInterval(this.pingInterval);
      this.pingInterval = null;
    }
  }
  
  disconnect() {
    this.stopPingInterval();
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
    this.connected = false;
  }
}

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
  module.exports = WebSocketClient;
}
