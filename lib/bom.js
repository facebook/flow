/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

/* BOM */


type PermissionState =
  | "granted"
  | "denied"
  | "prompt";

type FileSystemHandlePermissionDescriptor = {|
  mode: "read" | "readwrite";
|}

declare class PermissionStatus extends EventTarget {
  onchange: ?((event: any) => mixed);
  +state: PermissionState;
}

// https://www.w3.org/TR/hr-time-2/#dom-domhighrestimestamp
// https://developer.mozilla.org/en-US/docs/Web/API/DOMHighResTimeStamp
declare type DOMHighResTimeStamp = number;

declare class Location {
    ancestorOrigins: string[];
    hash: string;
    host: string;
    hostname: string;
    href: string;
    origin: string;
    pathname: string;
    port: string;
    protocol: string;
    search: string;
    assign(url: string): void;
    reload(flag?: boolean): void;
    replace(url: string): void;
    toString(): string;
}

///////////////////////////////////////////////////////////////////////////////

type FormDataEntryValue = string | File

declare class FormData {
    constructor(form?: HTMLFormElement, submitter?: HTMLElement | null): void;

    has(name: string): boolean;
    get(name: string): ?FormDataEntryValue;
    getAll(name: string): Array<FormDataEntryValue>;

    set(name: string, value: string): void;
    set(name: string, value: Blob, filename?: string): void;
    set(name: string, value: File, filename?: string): void;

    append(name: string, value: string): void;
    append(name: string, value: Blob, filename?: string): void;
    append(name: string, value: File, filename?: string): void;

    delete(name: string): void;

    keys(): Iterator<string>;
    values(): Iterator<FormDataEntryValue>;
    entries(): Iterator<[string, FormDataEntryValue]>;
}

declare class DOMRectReadOnly {
  static fromRect(rectangle?: {
    x: number,
    y: number,
    width: number,
    height: number,
    ...
  }): DOMRectReadOnly;
  constructor(x: number, y: number, width: number, height: number): void;
  +bottom: number;
  +height: number;
  +left: number;
  +right: number;
  +top: number;
  +width: number;
  +x: number;
  +y: number;
}

declare class DOMRect extends DOMRectReadOnly {
  static fromRect(rectangle?: {
    x: number,
    y: number,
    width: number,
    height: number,
    ...
  }): DOMRect;
  bottom: number;
  height: number;
  left: number;
  right: number;
  top: number;
  width: number;
  x: number;
  y: number;
}

declare class DOMRectList {
  @@iterator(): Iterator<DOMRect>;
  length: number;
  item(index: number): DOMRect;
  [index: number]: DOMRect;
}

type AudioContextState = 'suspended' | 'running' | 'closed';

declare class CanvasCaptureMediaStream extends MediaStream {
  canvas: HTMLCanvasElement;
  requestFrame(): void;
}

type DoubleRange = {
  max?: number;
  min?: number;
  ...
}

type LongRange = {
  max?: number;
  min?: number;
  ...
}

type ConstrainBooleanParameters = {
  exact?: boolean;
  ideal?: boolean;
  ...
}

type ConstrainDOMStringParameters = {
  exact?: string | string[];
  ideal?: string | string[];
  ...
}

type ConstrainDoubleRange = {
  ...DoubleRange;
  exact?: number;
  ideal?: number;
  ...
}

type ConstrainLongRange = {
  ...LongRange;
  exact?: number;
  ideal?: number;
  ...
}

type MediaTrackConstraintSet = {
  width?: number | ConstrainLongRange;
  height?: number | ConstrainLongRange;
  aspectRatio?: number | ConstrainDoubleRange;
  frameRate?: number | ConstrainDoubleRange;
  facingMode?: string | string[] | ConstrainDOMStringParameters;
  resizeMode?: string | string[] | ConstrainDOMStringParameters;
  volume?: number | ConstrainDoubleRange;
  sampleRate?: number | ConstrainLongRange;
  sampleSize?: number | ConstrainLongRange;
  echoCancellation?: boolean | ConstrainBooleanParameters;
  autoGainControl?: boolean | ConstrainBooleanParameters;
  noiseSuppression?: boolean | ConstrainBooleanParameters;
  latency?: number | ConstrainDoubleRange;
  channelCount?: number | ConstrainLongRange;
  deviceId?: string | string[] | ConstrainDOMStringParameters;
  groupId?: string | string[] | ConstrainDOMStringParameters;
  ...
}

type MediaTrackConstraints = {
  ...MediaTrackConstraintSet;
  advanced?: Array<MediaTrackConstraintSet>;
  ...
}

type MediaStreamConstraints = {
  audio?: boolean | MediaTrackConstraints;
  video?: boolean | MediaTrackConstraints;
  peerIdentity?: string;
  ...
}

type MediaTrackSettings = {
  aspectRatio?: number;
  deviceId?: string;
  displaySurface?: 'application' | 'browser' | 'monitor' | 'window';
  echoCancellation?: boolean;
  facingMode?: string;
  frameRate?: number;
  groupId?: string;
  height?: number;
  logicalSurface?: boolean;
  sampleRate?: number;
  sampleSize?: number;
  volume?: number;
  width?: number;
  ...
}

type MediaTrackCapabilities = {
  aspectRatio?: number | DoubleRange;
  deviceId?: string;
  echoCancellation?: boolean[];
  facingMode?: string;
  frameRate?: number | DoubleRange;
  groupId?: string;
  height?: number | LongRange;
  sampleRate?: number | LongRange;
  sampleSize?: number | LongRange;
  volume?: number | DoubleRange;
  width?: number | LongRange;
  ...
}

declare class MediaStream extends EventTarget {
  active: boolean;
  ended: boolean;
  id: string;
  onactive: (ev: any) => mixed;
  oninactive: (ev: any) => mixed;
  onended: (ev: any) => mixed;
  onaddtrack: (ev: MediaStreamTrackEvent) => mixed;
  onremovetrack: (ev: MediaStreamTrackEvent) => mixed;
  addTrack(track: MediaStreamTrack): void;
  clone(): MediaStream;
  getAudioTracks(): MediaStreamTrack[];
  getTrackById(trackid?: string): ?MediaStreamTrack;
  getTracks(): MediaStreamTrack[];
  getVideoTracks(): MediaStreamTrack[];
  removeTrack(track: MediaStreamTrack): void;
}

declare class MediaStreamTrack extends EventTarget {
  enabled: boolean;
  id: string;
  kind: string;
  label: string;
  muted: boolean;
  readonly: boolean;
  readyState: 'live' | 'ended';
  remote: boolean;
  contentHint?: string;
  onstarted: (ev: any) => mixed;
  onmute: (ev: any) => mixed;
  onunmute: (ev: any) => mixed;
  onoverconstrained: (ev: any) => mixed;
  onended: (ev: any) => mixed;
  getConstraints(): MediaTrackConstraints;
  applyConstraints(constraints?: MediaTrackConstraints): Promise<void>;
  getSettings(): MediaTrackSettings;
  getCapabilities(): MediaTrackCapabilities;
  clone(): MediaStreamTrack;
  stop(): void;
}

declare class MediaStreamTrackEvent extends Event {
  track: MediaStreamTrack;
}

declare class URLSearchParams {
    @@iterator(): Iterator<[string, string]>;

    size: number;

    constructor(init?: string | URLSearchParams | Array<[string, string]> | { [string]: string, ... } ): void;
    append(name: string, value: string): void;
    delete(name: string, value?: string): void;
    entries(): Iterator<[string, string]>;
    forEach<This>(callback: (this : This, value: string, name: string, params: URLSearchParams) => mixed, thisArg: This): void;
    get(name: string): null | string;
    getAll(name: string): Array<string>;
    has(name: string, value?: string): boolean;
    keys(): Iterator<string>;
    set(name: string, value: string): void;
    sort(): void;
    values(): Iterator<string>;
    toString(): string;
}

declare class AbortSignal extends EventTarget {
    +aborted: boolean;
    +reason: any;
    abort(reason?: any): AbortSignal;
    onabort: (event: Event) => mixed;
    throwIfAborted(): void;
    timeout(time: number): AbortSignal;
}

declare class MediaQueryListEvent {
  matches: boolean;
  media: string;
}

declare type MediaQueryListListener = MediaQueryListEvent => void;

/* Trusted Types
 * https://w3c.github.io/trusted-types/dist/spec/#trusted-types
 */
declare class TrustedHTML {
  toString(): string;
  toJSON(): string;
}

declare class TrustedScript {
  toString(): string;
  toJSON(): string;
}

declare class TrustedScriptURL {
  toString(): string;
  toJSON(): string;
}

// https://wicg.github.io/webusb/
// https://developer.mozilla.org/en-US/docs/Web/API/USBDevice
declare class USBDevice {
  configuration: USBConfiguration;
  configurations: Array<USBConfiguration>;
  deviceClass: number;
  deviceProtocol: number;
  deviceSubclass: number;
  deviceVersionMajor: number;
  deviceVersionMinor: number;
  deviceVersionSubminor: number;
  manufacturerName: ?string;
  opened: boolean;
  productId: number;
  productName: ?string;
  serialNumber: ?string;
  usbVersionMajor: number;
  usbVersionMinor: number;
  usbVersionSubminor: number;
  vendorId: number;
  claimInterface(interfaceNumber: number): Promise<void>;
  clearHalt(direction: 'in' | 'out', endpointNumber: number): Promise<void>;
  close(): Promise<void>;
  controlTransferIn(setup: SetUpOptions, length: number): Promise<USBInTransferResult>;
  controlTransferOut(setup: SetUpOptions, data: ArrayBuffer): Promise<USBOutTransferResult>;
  forget(): Promise<void>;
  isochronousTransferIn(endpointNumber: number, packetLengths: Array<number>): Promise<USBIsochronousInTransferResult>;
  isochronousTransferOut(endpointNumber: number, data: ArrayBuffer, packetLengths: Array<number>): Promise<USBIsochronousOutTransferResult>;
  open(): Promise<void>;
  releaseInterface(interfaceNumber: number): Promise<void>;
  reset(): Promise<void>;
  selectAlternateInterface(interfaceNumber: number, alternateSetting: number): Promise<void>;
  selectConfiguration(configurationValue: number): Promise<void>;
  transferIn(endpointNumber: number, length: number): Promise<USBInTransferResult>;
  transferOut(endpointNumber: number, data: ArrayBuffer): Promise<USBOutTransferResult>;
}

declare type USBDeviceFilter = {|
  vendorId?: number;
  productId?: number;
  classCode?: number;
  subclassCode?: number;
  protocolCode?: number;
  serialNumber?: string;
|};

declare type USBDeviceRequestOptions = {|
  filters: Array<USBDeviceFilter>;
  exclusionFilters?: Array<USBDeviceFilter>;
|}

declare class USBConfiguration {
  constructor(): void;
  configurationName: ?string;
  configurationValue: number;
  interfaces: $ReadOnlyArray<USBInterface>;

}

declare class USBInterface {
  constructor(): void;
  interfaceNumber: number;
  alternate: USBAlternateInterface;
  alternates: Array<USBAlternateInterface>;
  claimed: boolean;
}

declare class USBAlternateInterface {
  constructor(): void;
  alternateSetting: number;
  interfaceClass: number;
  interfaceSubclass: number;
  interfaceProtocol: number;
  interfaceName: ?string;
  endpoints : Array<USBEndpoint>;
}

declare class USBEndpoint {
  constructor(): void;
  endpointNumber: number;
  direction: 'in' | 'out';
  type:'bulk' | 'interrupt' | 'isochronous';
  packetSize: number;
}

declare class USBOutTransferResult {
  constructor(): void;
  bytesWritten: number;
  status: 'ok' | 'stall';
}

declare class USBInTransferResult {
  constructor(): void;
  data: DataView;
  status: 'ok' |'stall' | 'babble';
}

declare class USBIsochronousInTransferResult {
  constructor(): void;
  data: DataView;
  packets: Array<USBIsochronousInTransferPacket>;
}

declare class USBIsochronousInTransferPacket {
  constructor(): void;
  data: DataView;
  status: 'ok' |'stall' | 'babble';
}

declare class USBIsochronousOutTransferResult {
  constructor(): void;
  packets: Array<USBIsochronousOutTransferPacket>
}

declare class USBIsochronousOutTransferPacket {
  constructor(): void;
  bytesWritten: number;
  status: 'ok' | 'stall';
}

type SetUpOptions = {
  requestType: string;
  recipient: string;
  request: number;
  value: number;
  index: number;
  ...
}

declare type FileSystemHandleKind = "file" | "directory";

// https://wicg.github.io/file-system-access/#api-filesystemhandle
declare class FileSystemHandle {
  +kind: FileSystemHandleKind;
  +name: string;

  isSameEntry: (other: FileSystemHandle) => Promise<boolean>;
  queryPermission?: (
    descriptor: FileSystemHandlePermissionDescriptor
  ) => Promise<PermissionStatus>;
  requestPermission?: (
    descriptor: FileSystemHandlePermissionDescriptor
  ) => Promise<PermissionStatus>;
}

// https://fs.spec.whatwg.org/#api-filesystemfilehandle
declare class FileSystemFileHandle extends FileSystemHandle {
  +kind: "file";

  constructor(name: string): void;

  getFile(): Promise<File>;
  createSyncAccessHandle(): Promise<FileSystemSyncAccessHandle>;
  createWritable(options?: {|
    keepExistingData?: boolean,
  |}): Promise<FileSystemWritableFileStream>;
}

// https://fs.spec.whatwg.org/#api-filesystemdirectoryhandle
declare class FileSystemDirectoryHandle extends FileSystemHandle {
  +kind: "directory";

  constructor(name: string): void;

  getDirectoryHandle(
    name: string,
    options?: {| create?: boolean |}
  ): Promise<FileSystemDirectoryHandle>;
  getFileHandle(
    name: string,
    options?: {| create?: boolean |}
  ): Promise<FileSystemFileHandle>;
  removeEntry(name: string, options?: {| recursive?: boolean |}): Promise<void>;
  resolve(possibleDescendant: FileSystemHandle): Promise<Array<string> | null>;

  // Async iterator functions
  @@asyncIterator(): AsyncIterator<[string, FileSystemHandle]>;
  entries(): AsyncIterator<[string, FileSystemHandle]>;
  keys(): AsyncIterator<string>;
  values(): AsyncIterator<FileSystemHandle>;
}

// https://fs.spec.whatwg.org/#api-filesystemsyncaccesshandle
declare class FileSystemSyncAccessHandle {
  close(): void;
  flush(): void;
  getSize(): number;
  read(buffer: ArrayBuffer, options?: {| at: number |}): number;
  truncate(newSize: number): void;
  write(buffer: ArrayBuffer, options?: {| at: number |}): number;
}

// https://streams.spec.whatwg.org/#default-writer-class
declare class WritableStreamDefaultWriter {
  +closed: Promise<void>;
  +desiredSize: number;
  +ready: Promise<void>;

  constructor(): void;

  abort(reason?: string): Promise<void>;
  close(): Promise<void>;
  releaseLock(): void;
  write(chunk: any): Promise<void>;
}

// https://streams.spec.whatwg.org/#ws-class
declare class WriteableStream {
  +locked: boolean;

  constructor(): void;

  abort(reason: string): Promise<string>;
  close(): Promise<void>;
  getWriter(): WritableStreamDefaultWriter;
}

// https://fs.spec.whatwg.org/#dictdef-writeparams
declare type FileSystemWriteableFileStreamDataTypes =
  | ArrayBuffer
  | $TypedArray
  | DataView
  | Blob
  | string;

// https://fs.spec.whatwg.org/#dictdef-writeparams
declare type FileSystemWriteableFileStreamData =
  | FileSystemWriteableFileStreamDataTypes
  | {|
      type: "write",
      position?: number,
      data: FileSystemWriteableFileStreamDataTypes,
    |}
  | {|
      type: "seek",
      position: number,
      data: FileSystemWriteableFileStreamDataTypes,
    |}
  | {|
      type: "size",
      size: number,
    |};

// https://fs.spec.whatwg.org/#api-filesystemwritablefilestream
declare class FileSystemWritableFileStream extends WriteableStream {
  write(data: FileSystemWriteableFileStreamData): Promise<void>;
  truncate(size: number): Promise<void>;
  seek(position: number): Promise<void>;
}
