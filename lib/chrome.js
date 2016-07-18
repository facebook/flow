declare class chrome$chrome {
  hid: chrome$hid;
  runtime: chrome$runtime;
  storage: chrome$storage;
  app: chrome$app;
  browser: chrome$browser;
  sockets: chrome$sockets;
}

declare class chrome$sockets {
  udp: chrome$udp;
}

declare type chrome$udpProperties = {
  persistent?: boolean;
  name?: string;
  bufferSize?: number;
}

declare type chrome$udpCreateInfo = {
  socketId: number;
}

declare type chrome$udpSendInfo = {
  resultCode: number;
  bytesSent?: number;
}

declare class chrome$onReceive {
  addListener: (callback: (info: chrome$udpReceiveInfo) => void) => void;
}

declare type chrome$udpReceiveInfo = {
  socketId: number;
  data: ArrayBuffer;
  remoteAddress: string;
  remotePort: number;
}

declare class chrome$udp {
  create: (properties: chrome$udpProperties, callback: (info: chrome$udpCreateInfo) => void) => void;
  close: (socketId: number, callback: () => void) => void;
  bind: (socketId: number, address: string, port: number, callback: (result: number) => void) => void;
  send: (socketId: number, data: ArrayBuffer, address: string, port: number,
    callback: (info: chrome$udpSendInfo) => void
  ) => void;
  onReceive: chrome$onReceive;
}

declare class chrome$hidDeviceInfo {
  deviceId: number;
  vendorId: number;
  productId: number;
  collections: Array<{usagePage: number, usage: number, reportIds:Array<number>}>;
  maxInputReportSize: number;
  maxOutputReportSize: number;
  maxFeatureReportSize: number;
  reportDescriptor: ArrayBuffer;
}

declare type chrome$hidGetDevicesOptions = {
  vendorId? : number;
  productId? : number;
}

declare class chrome$hid {
  getDevices: (options: chrome$hidGetDevicesOptions,
              callback: (i: Array<chrome$hidDeviceInfo>) => void) => void;

  send: (connectionId: number, reportId: number, data: ArrayBuffer, callback: () => void) => void;
  
  receive: (connectionId: number, callback: (reportId: number, data: ArrayBuffer) => void) => void;

  connect: (deviceId: number, callback: (connection: {connectionId: number}) => void) => void;

  disconnect: (connectionId: number, callback: () => void) => void;
}

declare class chrome$platformInfo {
  os: string;
  arch: string;
  nacl_arch: string;
}

declare class chrome$runtime {
  lastError: string;
  getPlatformInfo(callback: (platformInfo: chrome$platformInfo) => void): void,
  onMessageExternal: chrome$onMessage;
  onMessage: chrome$onMessage;
  getManifest(): Object;
  id: string;
}

declare type chrome$storageItems = { [key:string]: any}
declare type chrome$callback = () => void;

declare class chrome$storageArea {
  getBytesInUse(callback: (bytesInUse: number) => void): void;
  getBytesInUse(keys: string, callback: (bytesInUse: number) => void): void;
  getBytesInUse(keys: string[], callback: (bytesInUse: number) => void): void;
  clear(callback?: chrome$callback): void;
  set(items: chrome$storageItems, callback?: chrome$callback): void;
  remove(keys: string, callback?: chrome$callback): void;
  remove(keys: string[], callback?: chrome$callback): void;
  get(callback: (items: chrome$storageItems) => void): void;
  get(keys: string, callback: (items: chrome$storageItems) => void): void;
  get(keys: string[], callback: (items: chrome$storageItems) => void): void;

  QUOTA_BYTES: number;
}

declare class chrome$storage {
  local: chrome$storageArea;
}

declare class chrome$messageSender {
  tabs: ?any;
  frameId: ?number;
  id: ?string;
  url: ?string;
  tlsChanelId: ?string;
}

declare class chrome$onMessage {
  addListener: (
    callback: (
      message: Object, sender: chrome$messageSender, sendResponse: (
        response: Object
      ) => void
    ) => boolean
  ) => void;
}

declare class chrome$app {
  runtime: chrome$appRuntime;   
  window: chrome$appWindow;
}

declare class chrome$appRuntime {
  onLaunched: chrome$appOnLaunched;        
}

declare class chrome$appOnLaunched {
  addListener: (
    callback: () => void //callback can have more parameters but I am not using them
  ) => void;
}

declare class chrome$browser {
  // according to specification, callback is optional, but in reality it's mandatory (as of now)
  openTab: (options: {url: string}, callback: () => void) => void
}

declare type chrome$boundsSpecification = {
  left? : number,
  top? : number,
  width? : number,
  height? : number,
  minWidth?: number,
  minHeight?: number,
  maxWidth?: number,
  maxHeight?: number
}

declare class chrome$appWindowOnClosed {
  addListener: ( callback: () => void ) => void
}

declare class chrome$appWindow {
  create: (url: string, options: {innerBounds: chrome$boundsSpecification}, callback: (createdWindow: chrome$appWindow) => void) => void;
  onClosed: chrome$appWindowOnClosed;
}

declare var chrome: chrome$chrome;

