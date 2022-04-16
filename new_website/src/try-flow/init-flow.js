/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import {load as initFlowLocally} from './flow-loader';

class Deferred {
  constructor() {
    this.promise = new Promise((resolve, reject) => {
      this.resolve = resolve;
      this.reject = reject;
    });
  }
}

const workerRegistry = {};
class FlowWorker {
  constructor(version) {
    this._version = version;
    this._pending = {};
    this._index = 0;

    const worker = (this._worker = new Worker(window.tryFlowWorker));
    worker.onmessage = ({data}) => {
      if (data.id && this._pending[data.id]) {
        if (data.err) {
          this._pending[data.id].reject(data.err);
        } else {
          this._pending[data.id].resolve(data.result);
        }
        delete this._pending[data.id];
      }
    };
    worker.onerror = function () {
      console.log('There is an error with your worker!');
    };

    // keep a reference to the worker, so that it doesn't get GC'd and killed.
    workerRegistry[version] = worker;
  }

  send(data) {
    const id = ++this._index;
    const version = this._version;
    this._pending[id] = new Deferred();
    this._worker.postMessage({id, version, ...data});
    return this._pending[id].promise;
  }
}

function initFlowWorker(version) {
  const worker = new FlowWorker(version);
  return worker.send({type: 'init'}).then(() => worker);
}

class AsyncLocalFlow {
  constructor(flow) {
    this._flow = flow;
  }

  checkContent(filename, body) {
    return Promise.resolve(this._flow.checkContent(filename, body));
  }

  typeAtPos(filename, body, line, col) {
    return Promise.resolve(this._flow.typeAtPos(filename, body, line, col));
  }

  supportsParse() {
    return Promise.resolve(this._flow.parse != null);
  }

  parse(body, options) {
    return Promise.resolve(this._flow.parse(body, options));
  }
}

class AsyncWorkerFlow {
  constructor(worker) {
    this._worker = worker;
  }

  checkContent(filename, body) {
    return this._worker.send({type: 'checkContent', filename, body});
  }

  typeAtPos(filename, body, line, col) {
    return this._worker.send({type: 'typeAtPos', filename, body, line, col});
  }

  supportsParse() {
    return this._worker.send({type: 'supportsParse'});
  }

  parse(body, options) {
    return this._worker.send({type: 'parse', body, options});
  }
}

export default function initFlow(version) {
  const useWorker = localStorage.getItem('tryFlowUseWorker');
  if (useWorker === 'true') {
    return initFlowWorker(version).then(flow => new AsyncWorkerFlow(flow));
  } else {
    return initFlowLocally(version).then(flow => new AsyncLocalFlow(flow));
  }
}
