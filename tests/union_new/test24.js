// @noflow

// scaling test for full type resolution

declare class C {
  addListener(event: string, listener: any): C;
  emit(event: string, ...args:Array<any>): boolean;
  listeners(event: string): Array<any>;
  listenerCount(event: string): number;
  on(event: string, listener: any): C;
  once(event: string, listener: any): C;
  removeAllListeners(event?: string): C;
  removeListener(event: string, listener: any): C;
  off(event: string, listener: any): C;
  setMaxListeners(n: number): void;
  rawListeners(event: string): Array<any>;
}

declare class D extends C {
  listen(port: number, hostname?: string, backlog?: number, callback?: any): D;
  listen(path: string, callback?: any): D;
  listen(handle: any, callback?: any): D;
  close(callback?: any): D;
  address(): number;
  connections: number;
  maxConnections: number;
  getConnections(callback: any): void;
  ref():  D;
  unref():  D;
}

0 as D | number;
