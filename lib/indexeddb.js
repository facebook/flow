/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

// Implemented by window & worker
declare interface IDBEnvironment {
  indexedDB: IDBFactory;
}

type IDBDirection = 'next' | 'nextunique' | 'prev' | 'prevunique';
type IDBTransactionDurability = 'default' | 'relaxed' | 'strict';

// Implemented by window.indexedDB & worker.indexedDB
declare interface IDBFactory {
  open(name: string, version?: number): IDBOpenDBRequest;
  deleteDatabase(name: string): IDBOpenDBRequest;
  /**
   * No support in Firefox, hence the method is marked as optional.
   * See https://developer.mozilla.org/en-US/docs/Web/API/IDBFactory/databases#browser_compatibility
   */
  databases?: () => Promise<Array<IDBDatabaseInfo>>;
  cmp(a: any, b: any): -1|0|1;
}

declare interface IDBDatabaseInfo {
  name: string;
  version: number;
}

declare interface IDBRequest extends EventTarget {
  result: IDBObjectStore;
  error: Error;
  source: ?(IDBIndex | IDBObjectStore | IDBCursor);
  transaction: IDBTransaction;
  readyState: 'pending'|'done';
  onerror: (err: any) => mixed;
  onsuccess: (e: any) => mixed;
}

declare interface IDBOpenDBRequest extends IDBRequest {
  onblocked: (e: any) => mixed;
  onupgradeneeded: (e: any) => mixed;
}

declare interface IDBTransactionOptions {
  durability?: IDBTransactionDurability;
}

declare interface IDBDatabase extends EventTarget {
  close(): void;
  createObjectStore(name: string, options?: {
    keyPath?: ?(string|string[]),
    autoIncrement?: boolean,
    ...
  }): IDBObjectStore;
  deleteObjectStore(name: string): void;
  transaction(
    storeNames: string | string[] | DOMStringList,
    mode?: 'readonly' | 'readwrite' | 'versionchange',
    options?: IDBTransactionOptions
  ): IDBTransaction;
  name: string;
  version: number;
  objectStoreNames: DOMStringList;
  onabort: (e: any) => mixed;
  onclose: (e: any) => mixed;
  onerror: (e: any) => mixed;
  onversionchange: (e: any) => mixed;
}

declare interface IDBTransaction extends EventTarget {
  abort(): void;
  db: IDBDatabase;
  +durability: IDBTransactionDurability;
  error: Error;
  mode: 'readonly'|'readwrite'|'versionchange';
  name: string;
  objectStore(name: string): IDBObjectStore;
  onabort: (e: any) => mixed;
  oncomplete: (e: any) => mixed;
  onerror: (e: any) => mixed;
}

declare interface IDBObjectStore {
  add(value: any, key?: any): IDBRequest;
  autoIncrement: boolean;
  clear(): IDBRequest;
  createIndex(indexName: string, keyPath: string|string[], optionalParameter?: {
    unique?: boolean,
    multiEntry?: boolean,
    ...
  }): IDBIndex;
  count(keyRange?: any|IDBKeyRange): IDBRequest;
  delete(key: any): IDBRequest;
  deleteIndex(indexName: string): void;
  get(key: any): IDBRequest;
  index(indexName: string): IDBIndex;
  indexNames: string[];
  name: string;
  keyPath: any;
  openCursor(range?: any|IDBKeyRange, direction?: IDBDirection): IDBRequest;
  openKeyCursor(range?: any|IDBKeyRange, direction?: IDBDirection): IDBRequest;
  put(value: any, key?: any): IDBRequest;
  transaction : IDBTransaction;
}

declare interface IDBIndex extends EventTarget {
  count(key?: any|IDBKeyRange): IDBRequest;
  get(key: any|IDBKeyRange): IDBRequest;
  getKey(key: any|IDBKeyRange): IDBRequest;
  openCursor(range?: any|IDBKeyRange, direction?: IDBDirection): IDBRequest;
  openKeyCursor(range?: any|IDBKeyRange, direction?: IDBDirection): IDBRequest;
  name: string;
  objectStore: IDBObjectStore;
  keyPath: any;
  multiEntry: boolean;
  unique: boolean;
}

declare interface IDBKeyRange {
  bound(lower: any, upper: any, lowerOpen?: boolean, upperOpen?: boolean): IDBKeyRange;
  only(value: any): IDBKeyRange;
  lowerBound(bound: any, open?: boolean): IDBKeyRange;
  upperBound(bound: any, open?: boolean): IDBKeyRange;
  lower: any;
  upper: any;
  lowerOpen: boolean;
  upperOpen: boolean;
}

declare interface IDBCursor {
  advance(count: number): void;
  continue(key?: any): void;
  delete(): IDBRequest;
  update(newValue: any): IDBRequest;
  source: IDBObjectStore|IDBIndex;
  direction: IDBDirection;
  key: any;
  primaryKey: any;
}

declare interface IDBCursorWithValue extends IDBCursor {
  value: any;
}
