const WebSocket = require('ws');

const ws = new WebSocket('ws://localhost:3838/websocket');

ws.on('open', function open() {
  console.log('✓ WebSocket connected');
  
  // Wait a bit then send input
  setTimeout(() => {
    console.log('Sending input: name = "Alice"');
    ws.send(JSON.stringify({
      type: 'user_input',
      data: {
        input_name: 'name',
        value: 'Alice'
      },
      timestamp: Date.now()
    }));
  }, 1000);
});

ws.on('message', function message(data) {
  const msg = JSON.parse(data.toString());
  console.log('Received message:', msg.type, msg.data);
  
  if (msg.type === 'value_update') {
    console.log('✓✓✓ VALUE UPDATE RECEIVED!');
    console.log('  Output:', msg.data.output_name);
    console.log('  Value:', msg.data.value);
  }
});

ws.on('error', function error(err) {
  console.error('WebSocket error:', err.message);
});

ws.on('close', function close() {
  console.log('WebSocket closed');
});

// Keep alive
setTimeout(() => {
  console.log('Closing connection...');
  ws.close();
  process.exit(0);
}, 5000);
