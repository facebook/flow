/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import {load as initFlowLocally} from './flow-loader';

class Deferred<T> {
  promise: Promise<T>;
  resolve: (Promise<T> | T) => void;
  reject: any => void;

  constructor() {
    this.promise = new Promise((resolve, reject) => {
      this.resolve = resolve;
      this.reject = reject;
    });
  }
}

const workerRegistry: {[string]: Worker} = {};
class FlowWorker {
  _version: string;
  _index: number;
  _pending: {[number]: any};
  _worker: Worker;

  constructor(version: string) {
    this._version = version;
    this._pending = {};
    this._index = 0;

    const worker = (this._worker = new Worker(window.tryFlowWorker));
    // $FlowFixMe[incompatible-type]
    worker.onmessage = ({data}: {data: any}) => {
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

  send(data: mixed) {
    const id = ++this._index;
    const version = this._version;
    this._pending[id] = new Deferred();
    this._worker.postMessage({id, version, ...data});
    return this._pending[id].promise;
  }
}

function initFlowWorker(version: string) {
  const worker = new FlowWorker(version);
  return worker.send({type: 'init'}).then(() => worker);
}

export interface AsyncFlow {
  checkContent(
    filename: string,
    body: string,
  ): Promise<$ReadOnlyArray<FlowJsError>>;
  typeAtPos(
    filename: string,
    body: string,
    line: number,
    col: number,
  ): Promise<string>;
  supportsParse(): Promise<boolean>;
  parse(body: string, options: FlowJsOptions): Promise<interface {}>;
}

class AsyncLocalFlow implements AsyncFlow {
  _flow: FlowJs;
  constructor(flow: FlowJs) {
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

class AsyncWorkerFlow implements AsyncFlow {
  _worker: FlowWorker;

  constructor(worker: FlowWorker) {
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

export default function initFlow(version: string): Promise<AsyncFlow> {
  const useWorker = localStorage.getItem('tryFlowUseWorker');
  if (useWorker === 'true') {
    return initFlowWorker(version).then(flow => new AsyncWorkerFlow(flow));
  } else {
    return initFlowLocally(version).then(flow => new AsyncLocalFlow(flow));
  }
}
