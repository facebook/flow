/**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

type FrameType = 'auxiliary' | 'top-level' | 'nested' | 'none';
type VisibilityState = 'hidden' | 'visible' | 'prerender' | 'unloaded';

declare class WindowClient extends Client {
  visibilityState: VisibilityState,
  focused: boolean,
  focus(): Promise<WindowClient>,
  navigate(url: string): Promise<WindowClient>,
}

declare class Client {
  id: string,
  reserved: boolean,
  url: string,
  frameType: FrameType,
  postMessage(message: any, transfer?: Iterator<any> | Array<any>): void,
}

declare class ExtendableEvent extends Event {
  waitUntil(f: Promise<mixed>): void,
}

type ForeignFetchOptions = {
  scopes: Iterator<string>,
  origins: Iterator<string>,
};

declare class InstallEvent extends ExtendableEvent {
  registerForeignFetch(options: ForeignFetchOptions): void,
}

declare class FetchEvent extends ExtendableEvent {
  request: Request,
  client: Client, // client is deprecated
  clientId: string,
  isReload: boolean,
  respondWith(response: Response | Promise<Response>): void,
}

type ClientType = 'window' | 'worker' | 'sharedworker' | 'all';
type ClientQueryOptions = {
  includeUncontrolled?: boolean,
  includeReserved?: boolean,
  type?: ClientType,
};

declare class Clients {
  get(id: string): Promise<?Client>,
  matchAll(options?: ClientQueryOptions): Promise<Iterator<Client>>,
  openWindow(url: string): Promise<?WindowClient>,
  claim(): Promise<void>,
}

type ServiceWorkerState = 'installing'
  | 'installed'
  | 'activating'
  | 'activated'
  | 'redundant';

declare class ServiceWorker extends EventTarget {
  scriptURL: string,
  state: ServiceWorkerState,

  postMessage(message: any, transfer?: Iterator<any>): void,

  onstatechange?: EventHandler,
}

declare class ServiceWorkerRegistration extends EventTarget {
  installing: ?ServiceWorker,
  waiting: ?ServiceWorker,
  active: ?ServiceWorker,

  scope: string,

  update(): Promise<void>,
  unregister(): Promise<boolean>,

  onupdatefound?: EventHandler,
}

type WorkerType = 'classic' | 'module';

type RegistrationOptions = {
  scope: string,
  type: WorkerType,
};

declare class ServiceWorkerContainer extends EventTarget {
  controller: ?ServiceWorker,
  ready: Promise<ServiceWorkerRegistration>,

  getRegistration(clientURL: string): Promise<?ServiceWorkerRegistration>,
  getRegistrations(): Promise<Iterator<ServiceWorkerRegistration>>,
  register(
    scriptURL: string,
    options: ?RegistrationOptions
  ): Promise<ServiceWorkerRegistration>,
  startMessages(): void,

  oncontrollerchange?: EventHandler,
  onmessage?: EventHandler,
}

declare class ServiceWorkerMessageEvent {
  data: any,
  lastEventId :string,
  origin: string,
  ports: Array<MessagePort>,
  source: ?(ServiceWorker | MessagePort),
}

type CacheQueryOptions = {
  ignoreSearch?: boolean,
  ignoreMethod?: boolean,
  ignoreVary?: boolean,
  cacheName?: string,
}

type RequestInfo = Request | string;

declare class Cache {
  match(request: RequestInfo, options?: CacheQueryOptions): Promise<Response>,
  matchAll(
    request: RequestInfo,
    options?: CacheQueryOptions
  ): Promise<Array<Response>>,
  add(request: RequestInfo): Promise<void>,
  addAll(requests: Array<RequestInfo>): Promise<void>,
  put(request: RequestInfo, response: Response): Promise<void>,
  delete(request: RequestInfo, options?: CacheQueryOptions): Promise<boolean>,
  keys(
    request?: RequestInfo,
    options?: CacheQueryOptions
  ): Promise<Array<Request>>,
}

declare class CacheStorage {
  match(request: RequestInfo, options?: CacheQueryOptions): Promise<Response>,
  has(cacheName: string): Promise<true>,
  open(cacheName: string): Promise<Cache>,
  delete(cacheName: string): Promise<boolean>,
  keys(): Promise<Array<string>>,
}

// Service worker global scope
// https://www.w3.org/TR/service-workers/#service-worker-global-scope
declare var clients: Clients;
declare var caches: CacheStorage;
declare var registration: ServiceWorkerRegistration;
declare function skipWaiting(): Promise<void>;
declare var onactivate: ?EventHandler;
declare var oninstall: ?EventHandler;
declare var onfetch: ?EventHandler;
declare var onforeignfetch: ?EventHandler;
declare var onmessage: ?EventHandler;
