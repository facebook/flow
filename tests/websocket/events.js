const ws = new WebSocket('http://foo/bar');

function handleClose(event: CloseEvent) {
  console.log('WebSocket closed with code', event.code);
}

function handleMessage(event: MessageEvent) {
  console.log('WebSocket received message', event.data);
}

ws.addEventListener('message', handleMessage);
ws.addEventListener('close', handleClose);
ws.onmessage = handleMessage;
ws.onclose = handleClose;
