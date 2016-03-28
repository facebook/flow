// Implemented by window & worker
declare interface IDBEnvironment {
  indexedDB: IDBFactory;
}

type IDBDirection = 'next' | 'nextunique' | 'prev' | 'prevunique';

// Implemented by window.indexedDB & worker.indexedDB
declare interface IDBFactory {
  open(name: string, version?: number): IDBOpenDBRequest;
  deleteDatabase(name: string): IDBOpenDBRequest;
  cmp(a: any, b: any): -1|0|1;
}

declare interface IDBRequest extends EventTarget {
  result: IDBObjectStore;
  error: Error;
  source: ?(IDBIndex | IDBObjectStore | IDBCursor);
  transaction: IDBTransaction;
  readyState: 'pending'|'done';
  onerror: (err: any) => void;
  onsuccess: (e: any) => void;
}

declare interface IDBOpenDBRequest extends IDBRequest {
  onblocked: (e: any) => void;
  onupgradeneeded: (e: any) => void;
}

declare interface IDBDatabase extends EventTarget {
  close(): void;
  createObjectStore(name: string, options?: {
    keyPath?: string|string[],
    autoIncrement?: bool
  }): IDBObjectStore;
  deleteObjectStore(name: string): void;
  transaction(storeNames: string|string[], mode?: 'readonly'|'readwrite'|'readwriteflush'): IDBTransaction;
  name: string;
  version: number;
  objectStoreNames: string[];
  onabort: (e: any) => void;
  onerror: (e: any) => void;
  onversionchange: (e: any) => void;
}

declare interface IDBTransaction extends EventTarget {
  abort(): void;
  objectStore(name: string): IDBObjectStore;
  db: IDBDatabase;
  mode: 'readonly'|'readwrite'|'versionchange';
  objectStoreNames: string[];
  error: Error;
  onabort: (e: any) => void;
  onerror: (e: any) => void;
  oncomplete: (e: any) => void;
}

declare interface IDBObjectStore {
  add(value: any, key?: any): IDBRequest;
  clear(): IDBRequest;
  delete(key: any): IDBRequest;
  get(key: any): IDBRequest;
  getAll(query?: any|IDBKeyRange, count?: number): IDBRequest;
  getAllKeys(query?: IDBKeyRange, count?: number): IDBRequest;
  createIndex(indexName: string, keyPath: ?(string|string[]), optionalParameter?: {
    unique: bool,
    multiEntry: bool,
  }): IDBIndex;
  deleteIndex(indexName: string): void;
  index(indexName: string): IDBIndex;
  put(value: any, key?: any): IDBRequest;
  openCursor(range?: any|IDBKeyRange, direction?: IDBDirection): IDBRequest;
  openKeyCursor(range?: any|IDBKeyRange, direction?: IDBDirection): IDBRequest;
  count(keyRange?: any|IDBKeyRange): IDBRequest;
  indexNames: string[];
  keyPath: any;
  transaction : IDBTransaction;
  autoIncrement: bool;
}

declare interface IDBIndex extends EventTarget {
  count(key?: any|IDBKeyRange): IDBRequest;
  get(key?: any|IDBKeyRange): IDBRequest;
  getKey(key?: any|IDBKeyRange): IDBRequest;
  openCursor(range?: any|IDBKeyRange, direction?: IDBDirection): IDBRequest;
  openKeyCursor(range?: any|IDBKeyRange, direction?: IDBDirection): IDBRequest;
  name: string;
  objectStore: IDBObjectStore;
  keyPath: any;
  multiEntry: bool;
  unique: bool;
}

declare interface IDBKeyRange {
  bound(lower: any, upper: any, lowerOpen?: bool, upperOpen?: bool): IDBKeyRange;
  only(value: any): IDBKeyRange;
  lowerBound(bound: any, open?: bool): IDBKeyRange;
  upperBound(bound: any, open?: bool): IDBKeyRange;
  lower: any;
  upper: any;
  lowerOpen: any;
  upperOpen: any;
}

declare interface IDBCursor {
  advance(count: number): void;
  continue(key?: any): void;
  delete(): IDBRequest;
  update(newValue: any): IDBRequest;
  source: IDBObjectStore|IDBIndex;
  direction: IDBDatabase;
  key: ?any;
  primaryKey: ?any;
}

declare interface IDBCursorWithValue extends IDBCursor {
  value: any;
}
