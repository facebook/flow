// @flow

var worker = new Worker('some/path');

worker.addEventListener('message', (ev: MessageEvent) => {});
worker.addEventListener('error', (ev: Event) => {});

worker.onerror = (ev: Event) => {};
worker.onmessage = (ev: MessageEvent) => {};
