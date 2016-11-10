/**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */
/* BOM */

declare class Screen {
  availHeight: number;
  availLeft: number;
  availTop: number;
  availWidth: number;
  colorDepth: number;
  height: number;
  left: number;
  mozOrientation?: string;
  onmozorientationchange?: any;
  orientation?: {
    lock(): Promise<void>;
    unlock(): void;
    angle: number;
    onchange: () => mixed;
    type: 'portrait-primary' | 'portrait-secondary' | 'landscape-primary' | 'landscape-secondary';
  };
  pixelDepth: number;
  top: number;
  width: number;
  mozLockOrientation?: Function;
  mozUnlockOrientation?: Function;
  mozOrientation?: string;
  onmozorientationchange?: Function;
}

declare var screen: Screen;
declare var window: any;

type GamepadButton = {
  pressed: bool;
  value: number;
}
type Gamepad = {
  axes: number[];
  buttons: GamepadButton[];
  connected: bool;
  id: string;
  index: number;
  mapping: string;
  timestamp: number;
}
declare class NavigatorCommon {
    appName: 'Netscape';
    appVersion: string;
    platform: string;
    userAgent: string;
    language: string;
    languages: Array<string>;
    onLine: boolean;
    hardwareConcurrency: number;
}

declare class Navigator mixins NavigatorCommon {
    appCodeName: 'Mozilla';
    buildID: string;
    cookieEnabled: boolean;
    doNotTrack?: any;
    geolocation: Geolocation;
    mediaDevices?: Object;
    javaEnabled: Function;
    maxTouchPoints: number;
    mimeTypes: MimeTypeArray;
    oscpu: string;
    permissions: any;
    plugins: PluginArray;
    product: 'Gecko';
    productSub: '20030107' | '20100101';
    serviceWorker?: Object;
    vendor: '' | 'Google Inc.' | 'Apple Computer, Inc';
    vendorSub: '';
    getBattery?: () => Promise<Gamepad>;
    getGamepads?: () => Object[];
    webkitGetGamepads?: Function;
    mozGetGamepads?: Function;
    mozGamepads?: any;
    gamepads?: any;
    webkitGamepads?: any;
    requestMIDIAccess?: Function;
    registerContentHandler(mimeType: string, uri: string, title: string): void;
    registerProtocolHandler(protocol: string, uri: string, title: string): void;
    requestMediaKeySystemAccess?: (keySystem: string, supportedConfigurations: any[]) => Promise<any>;
    sendBeacon?: Function;
    getUserMedia?: Function;
    webkitGetUserMedia?: Function;
    mozGetUserMedia?: Function;
    msGetUserMedia?: Function;
    taintEnabled?: Function;
    vibrate?: (pattern: number|number[]) => bool;
}

declare var navigator: Navigator;

declare class MimeType {
    type: string;
    description: string;
    suffixes: string;
    enabledPlugin: Plugin;
}

declare class MimeTypeArray {
    length: number;
    item(index: number): MimeType;
    namedItem(name: string): MimeType;
    [key: number | string]: MimeType;
}

declare class Plugin {
    description: string;
    filename: string;
    name: string;
    version?: string; // Gecko only
    length: number;
    item(index: number): MimeType;
    namedItem(name: string): MimeType;
    [key: number | string]: MimeType;
}

declare class PluginArray {
    length: number;
    item(index: number): Plugin;
    namedItem(name: string): Plugin;
    refresh(): void;
    [key: number | string]: Plugin;
}

declare class PerformanceTiming {
    connectEnd: number;
    connectStart: number;
    domainLookupEnd: number;
    domainLookupStart: number;
    domComplete: number;
    domContentLoadedEventEnd: number;
    domContentLoadedEventStart: number;
    domInteractive: number;
    domLoading: number;
    fetchStart: number;
    loadEventEnd: number;
    loadEventStart: number;
    navigationStart: number;
    redirectEnd: number;
    redirectStart: number;
    requestStart: number;
    responseEnd: number;
    responseStart: number;
    secureConnectionStart: number;
    unloadEventEnd: number;
    unloadEventStart: number;
}

declare class PerformanceNavigation {
    TYPE_NAVIGATE: 0;
    TYPE_RELOAD: 1;
    TYPE_BACK_FORWARD: 2;
    TYPE_RESERVED: 255;

    type: 0 | 1 | 2 | 255;
    redirectCount: number;
}

type PerformanceEntryFilterOptions = {
    name: string;
    entryType: string;
    initiatorType: string;
}

declare class PerformanceEntry {
    name: string;
    entryType: string;
    startTime: number;
    duration: number;
    toJSON(): string;
}

declare class Performance {
    navigation: PerformanceNavigation;
    onresourcetimingbufferfull: (ev: Event) => any;
    timing: PerformanceTiming;
    clearMarks(name?: string): void;
    clearMeasures(name?: string): void;
    clearResourceTimings(): void;
    getEntries(options?: PerformanceEntryFilterOptions): Array<PerformanceEntry>;
    getEntriesByName(name: string, type?: string): Array<PerformanceEntry>;
    getEntriesByType(type: string): Array<PerformanceEntry>;
    mark(name: string): void;
    measure(name: string, startMark?: string, endMark?: string): void;
    now(): number;
    setResourceTimingBufferSize(maxSize: number): void;
    toJSON(): string;
}

declare var performance: Performance;

declare class History {
    length: number;
    scrollRestoration: 'auto' | 'manual';
    state: any;
    back(): void;
    forward(): void;
    go(delta?: any): void;
    pushState(statedata: any, title: string, url?: string): void;
    replaceState(statedata: any, title: string, url?: string): void;
}

declare var history: History;

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

declare var location: Location;

///////////////////////////////////////////////////////////////////////////////

declare class DOMParser {
    parseFromString(source: string, mimeType: string): Document;
}

declare class FormData {
    append(name: any, value: any, blobName?: string): void;
}

declare class MutationRecord {
    type: 'attributes' | 'characterData' | 'childList';
    target: Node;
    addedNodes: NodeList<Node>;
    removedNodes: NodeList<Node>;
    previousSibling: ?Node;
    nextSibling: ?Node;
    attributeName: ?string;
    attributeNamespace: ?string;
    oldValue: ?string;
}

declare class MutationObserver {
    constructor(callback: (arr: Array<MutationRecord>, observer: MutationObserver)=>any): void;
    observe(target: Node, options: {
        childList?: boolean;
        attributes?: boolean;
        characterData?: boolean;
        subtree?: boolean;
        attributeOldValue?: boolean;
        characterDataOldValue?: boolean;
        attributeFilter?: Array<string>;
    }): void;
    takeRecords(): Array<MutationRecord>;
    disconnect(): void;
}

declare var NodeFilter: {
    acceptNode(n: Node): number;
    SHOW_ENTITY_REFERENCE: number;
    SHOW_NOTATION: number;
    SHOW_ENTITY: number;
    SHOW_DOCUMENT: number;
    SHOW_PROCESSING_INSTRUCTION: number;
    FILTER_REJECT: number;
    SHOW_CDATA_SECTION: number;
    FILTER_ACCEPT: number;
    SHOW_ALL: number;
    SHOW_DOCUMENT_TYPE: number;
    SHOW_TEXT: number;
    SHOW_ELEMENT: number;
    SHOW_COMMENT: number;
    FILTER_SKIP: number;
    SHOW_ATTRIBUTE: number;
    SHOW_DOCUMENT_FRAGMENT: number;
};

declare class CloseEvent extends Event {
    code: number;
    reason: string;
    wasClean: boolean;
}

declare class WebSocket extends EventTarget {
    static CONNECTING: 0;
    static OPEN: 1;
    static CLOSING: 2;
    static CLOSED: 3;
    constructor(url: string, protocols?: string | Array<string>): void;
    protocol: string;
    readyState: number;
    bufferedAmount: number;
    onopen: (ev: Event) => any;
    extensions: string;
    onmessage: (ev: MessageEvent) => any;
    onclose: (ev: CloseEvent) => any;
    onerror: (ev: Event) => any;
    binaryType: string;
    url: string;
    close(code?: number, reason?: string): void;
    send(data: any): void;
    CONNECTING: 0;
    OPEN: 1;
    CLOSING: 2;
    CLOSED: 3;
}

declare class Worker extends EventTarget {
    constructor(stringUrl: string): void;
    onerror: (ev: Event) => any;
    onmessage: (ev: MessageEvent) => any;
    postMessage(message: any, ports?: any): void;
    terminate(): void;
}

declare class SharedWorker extends EventTarget {
    constructor(stringUrl: string): void;
    port: MessagePort;
    onerror: (ev: Event) => any;
}

declare function importScripts(...urls: Array<string>): void;

declare class WorkerGlobalScope extends EventTarget {
    self: WorkerGlobalScope;
    location: WorkerLocation;
    navigator: WorkerNavigator;
    close(): void;
    importScripts(...urls: Array<string>): void;
    onerror: (ev: Event) => any;
    onlanguagechange: (ev: Event) => any;
    onoffline: (ev: Event) => any;
    ononline: (ev: Event) => any;
    onrejectionhandled: (ev: PromiseRejectionEvent) => any;
    onunhandledrejection: (ev: PromiseRejectionEvent) => any;
}

declare class DedicatedWorkerGlobalScope extends WorkerGlobalScope {
    onmessage(): (ev: MessageEvent) => any;
    postMessage(message: any, transfer?: Iterable<Object>): void;
}

declare class SharedWorkerGlobalScope extends WorkerGlobalScope {
    name: string;
    onconnect: (ev: MessageEvent) => any;
}

declare class WorkerLocation {
    origin: string;
    protocol: string;
    host: string;
    hostname: string;
    port: string;
    pathname: string;
    search: string;
    hash: string;
}

declare class WorkerNavigator mixins NavigatorCommon {}

declare class XDomainRequest {
    timeout: number;
    onerror: (ev: Event) => any;
    onload: (ev: Event) => any;
    onprogress: (ev: Event) => any;
    ontimeout: (ev: Event) => any;
    responseText: string;
    contentType: string;
    open(method: string, url: string): void;
    abort(): void;
    send(data?: any): void;
    addEventListener(type: string, listener: (evt: any) => void, useCapture?: boolean): void;

    statics: {
        create(): XDomainRequest;
    }
}

declare class XMLHttpRequest extends EventTarget {
    responseBody: any;
    status: number;
    readyState: number;
    responseText: string;
    responseXML: any;
    ontimeout: (ev: ProgressEvent) => any;
    statusText: string;
    onreadystatechange: (ev: Event) => any;
    timeout: number;
    onload: (ev: ProgressEvent) => any;
    response: any;
    withCredentials: boolean;
    onprogress: (ev: ProgressEvent) => any;
    onabort: (ev: ProgressEvent) => any;
    responseType: string;
    onloadend: (ev: ProgressEvent) => any;
    upload: XMLHttpRequestEventTarget;
    onerror: (ev: ProgressEvent) => any;
    onloadstart: (ev: ProgressEvent) => any;
    msCaching: string;
    open(method: string, url: string, async?: boolean, user?: string, password?: string): void;
    send(data?: any): void;
    abort(): void;
    getAllResponseHeaders(): string;
    setRequestHeader(header: string, value: string): void;
    getResponseHeader(header: string): string;
    msCachingEnabled(): boolean;
    overrideMimeType(mime: string): void;
    LOADING: number;
    DONE: number;
    UNSENT: number;
    OPENED: number;
    HEADERS_RECEIVED: number;

    statics: {
        create(): XMLHttpRequest;
    }
}

declare class XMLHttpRequestEventTarget extends EventTarget {
    onprogress: (ev: ProgressEvent) => any;
    onerror: (ev: Event) => any;
    onload: (ev: Event) => any;
    ontimeout: (ev: Event) => any;
    onabort: (ev: Event) => any;
    onloadstart: (ev: Event) => any;
    onloadend: (ev: Event) => any;
}

declare class XMLSerializer {
    serializeToString(target: Node): string;
}

declare class Geolocation {
    getCurrentPosition: (
        success: (position: Position) => any,
        error?: (error: PositionError) => any,
        options?: PositionOptions
    ) => any;
    watchPosition: (
        success: (position: Position) => any,
        error?: (error: PositionError) => any,
        options?: PositionOptions
    ) => any;
    clearWatch: any;
}

declare class Position {
    coords: Coordinates;
    timestamp: number;
}

declare class Coordinates {
    latitude: number;
    longitude: number;
    altitude?: number;
    accuracy: number;
    altitudeAccuracy?: number;
    heading?: number;
}

declare class PositionError {
    code: number;
    message: string;
    PERMISSION_DENIED: number;
    POSITION_UNAVAILABLE: number;
    TIMEOUT: number;
}

type PositionOptions = {
    enableHighAccuracy: boolean;
    timeout: number;
    maximumAge: number;
}

declare class AudioContext {
  currentTime: number;
  destination: AudioDestinationNode;
  listener: AudioListener;
  sampleRate: number;
  state: any;
  onstatechange: (ev: any) => any;
  close(): void;
  createBuffer(numOfChannels: number, length: number, sampleRate: number): AudioBuffer;
  createBufferSource(myMediaElement?: HTMLMediaElement): AudioBufferSourceNode;
  createMediaElementSource(myMediaElement: HTMLMediaElement): MediaElementAudioSourceNode;
  createMediaStreamSource(): MediaStreamAudioSourceNode;
  createMediaStreamDestination(): MediaStream;
  createScriptProcessor(bufferSize: number, numberOfInputChannels: number, numberOfOutputChannels: number): ScriptProcessorNode;
  createAnalyser(): AnalyserNode;
  createBiquadFilter(): BiquadFilterNode;
  createChannelMerger(numberOfInputs?: number): ChannelMergerNode;
  createChannelSplitter(numberOfInputs?: number): ChannelSplitterNode;
  createConvolver(): ConvolverNode;
  createDelay(maxDelayTime?: number): DelayNode;
  createDynamicCompressor(): DynamicsCompressorNode;
  createGain(): GainNode;
  createOscillator(): OscillatorNode;
  createPanner(): PannerNode;
  createPeriodicWave(real: Float32Array, img: Float32Array, options?: {
    disableNormalization: bool,
  }): PeriodicWave;
  createWaveShaper(): WaveShaperNode;
  decodeAudioData(arrayBuffer: ArrayBuffer, decodeSuccessCallback: Function, decodeErrorCallback: Function): void;
  decodeAudioData(arrayBuffer: ArrayBuffer): Promise<AudioBuffer>;
  resume(): Promise<void>;
  suspend(): Promise<void>;
}

declare class AudioNode {
  context: AudioContext;
  numberOfInputs: number;
  numberOfOutputs: number;
  channelCount: number;
  channelCoundMode: any;
  channelInterpretation: 'speakers'|'discrete';
  connect(audioNode: AudioNode, output?: number, input?: number): AudioNode;
  connect(destination: AudioParam, output?: number): void;
  disconnect(destination?: AudioNode, output?: number, input?: number): void;
}

declare class AudioParam extends AudioNode {
  value: number;
  defaultValue: number;
  setValueAtTime(value: number, startTime: number): this;
  linearRampToValueAtTime(value: number, endTime: number): this;
  exponentialRampToValueAtTime(value: number, endTime: number): this;
  setTargetAtTime(target: number, startTime: number, timeConstant: number): this;
  setValueCurveAtTime(values: Float32Array, startTime: number, duration: number): this;
  cancelScheduledValues(startTime: number): this;
}

declare class AudioDestinationNode extends AudioNode {
  maxChannelCount: number;
}

declare class AudioListener extends AudioNode {
  setOrientation(x: number, y: number, z: number, xUp: number, yUp: number, zUp: number): void;
}

declare class AudioBuffer {
  sampleRate: number;
  length: number;
  duration: number;
  numberOfChannels: number;
  getChannelData(channel: number): Float32Array;
  copyFromChannel(destination: Float32Array, channelNumber: number, startInChannel?: number): void;
  copyToChannel(source: Float32Array, channelNumber: number, startInChannel?: number): void;
}

declare class AudioBufferSourceNode extends AudioNode {
  buffer: AudioBuffer;
  detune: AudioParam;
  loop: bool;
  loopStart: number;
  loopEnd: number;
  playbackRate: AudioParam;
  onended: (ev: any) => any;
  start(when?: number, offset?: number, duration?: number): void;
  stop(when?: number): void;
}

declare class MediaStream extends EventTarget {
  active: bool;
  ended: bool;
  id: string;
  onactive: (ev: any) => any;
  onaddtrack: (ev: any) => any;
  onended: (ev: any) => any;
  oninactive: (ev: any) => any;
  onremovetrack: (ev: any) => any;
  addTrack(track: MediaStreamTrack): void;
  clone(): MediaStream;
  getAudioTracks(): MediaStreamTrack[];
  getTrackById(trackid?: string): ?MediaStreamTrack;
  getTracks(): MediaStreamTrack[];
  getVideoTracks(): MediaStreamTrack[];
  removeTrack(track: MediaStreamTrack): void;
}

declare class MediaStreamTrack {
  enabled: bool;
  id: string;
  kind: string;
  label: string;
  muted: bool;
  readonly: bool;
  readyState: 'live'|'ended';
  remote: bool;
  onstarted: (ev: any) => any;
  onmute: (ev: any) => any;
  onunmute: (ev: any) => any;
  onoverconstrained: (ev: any) => any;
  onended: (ev: any) => any;
  getConstraints(): any;
  applyConstraints(): any;
  getSettings(): any;
  getCapabilities(): any;
  clone(): MediaStreamTrack;
  stop(): void;
}

declare class MediaElementAudioSourceNode extends AudioNode {}
declare class MediaStreamAudioSourceNode extends AudioNode {}

declare class ScriptProcessorNode extends AudioNode {
  bufferSize: number;
  onaudioprocess: (ev: any) => any;
}

declare class AnalyserNode extends AudioNode {
  fftSize: number;
  frequencyBinCount: number;
  minDecibels: number;
  maxDecibels: number;
  smoothingTimeConstant: number;
  getFloatFrequencyData(array: Float32Array): Float32Array;
  getByteFrequencyData(array: Uint8Array): Uint8Array;
  getFloatTimeDomainData(array: Float32Array): Float32Array;
  getByteTimeDomainData(array: Uint8Array): Uint8Array;
}

declare class BiquadFilterNode extends AudioNode {
  frequency: AudioParam;
  detune: AudioParam;
  Q: AudioParam;
  gain: AudioParam;
  type: 'lowpass'|'highpass'|'bandpass'|'lowshelf'|'highshelf'|'peaking'|'notch'|'allpass';
  getFrequencyResponse(frequencyHz: Float32Array, magResponse: Float32Array, phaseResponse: Float32Array): BiquadFilterNode;
}

declare class ChannelMergerNode extends AudioNode {}
declare class ChannelSplitterNode extends AudioNode {}
declare class ConvolverNode extends AudioNode {
  buffer: AudioBuffer;
  normalize: bool;
}

declare class DelayNode extends AudioNode {
  delayTime: number;
}

declare class DynamicsCompressorNode extends AudioNode {
  threshold: AudioParam;
  knee: AudioParam;
  ratio: AudioParam;
  reduction: AudioParam;
  attack: AudioParam;
  release: AudioParam;
}

declare class GainNode extends AudioNode {
  gain: AudioParam;
}

declare class OscillatorNode extends AudioNode {
  frequency: AudioParam;
  detune: AudioParam;
  type: 'sine' | 'square' | 'sawtooth' | 'triangle' | 'custom';
  start(when?: number): void;
  stop(when?: number): void;
  setPeriodicWave(periodicWave: PeriodicWave): void;
}

declare class PannerNode extends AudioNode {
  panningModel: 'equalpower'|'HRTF';
  distanceModel: 'linear'|'inverse'|'exponential';
  refDistance: number;
  maxDistance: number;
  rollofFactor: number;
  coneInnerAngle: number;
  coneOuterAngle: number;
  coneOuterGain: number;
  setPosition(x: number, y: number, z: number): void;
  setOrientation(x: number, y: number, z: number): void;
}

declare class PeriodicWave extends AudioNode {}
declare class WaveShaperNode extends AudioNode {
  curve: Float32Array;
  oversample: 'none'|'2x'|'4x';
}


// this part of spec is not finished yet, apparently
// https://stackoverflow.com/questions/35296664/can-fetch-get-object-as-headers
type HeadersInit = Headers | {[key: string]: string};


// TODO Heades and URLSearchParams are almost the same thing.
// Could it somehow be abstracted away?
declare class Headers {
    @@iterator(): Iterator<[string, string]>;
    constructor(init?: HeadersInit): void;
    append(name: string, value: string): void;
    delete(name: string): void;
    entries(): Iterator<[string, string]>;
    get(name: string): string;
    getAll(name: string): Array<string>;
    has(name: string): boolean;
    keys(): Iterator<string>;
    set(name: string, value: string): void;
    values(): Iterator<string>;
}

declare class URLSearchParams {
    @@iterator(): Iterator<[string, string]>;
    constructor(query?: string | URLSearchParams): void;
    append(name: string, value: string): void;
    delete(name: string): void;
    entries(): Iterator<[string, string]>;
    get(name: string): string;
    getAll(name: string): Array<string>;
    has(name: string): boolean;
    keys(): Iterator<string>;
    set(name: string, value: string): void;
    values(): Iterator<string>;
}

type CacheType =  'default' | 'no-store' | 'reload' | 'no-cache' | 'force-cache' | 'only-if-cached';
type CredentialsType = 'omit' | 'same-origin' | 'include';
type ModeType = 'cors' | 'no-cors' | 'same-origin';
type RedirectType = 'follow' | 'error' | 'manual';
type MethodType = 'GET' | 'POST' | 'DELETE' | 'HEAD' | 'OPTIONS' | 'PUT' | 'PATCH' ;
type ReferrerPolicyType =
    '' | 'no-referrer' | 'no-referrer-when-downgrade' | 'origin-only' |
    'origin-when-cross-origin' | 'unsafe-url';
type ResponseType =  'basic' | 'cors' | 'default' | 'error' | 'opaque' | 'opaqueredirect' ;

type RequestOptions = {
    body?: ?(Blob | FormData | URLSearchParams | string);

    cache?: ?CacheType;
    credentials?: ?CredentialsType;
    headers?: ?HeadersInit;
    integrity?: ?string;
    method?: ?MethodType;
    mode?: ?ModeType;
    redirect?: ?RedirectType;
    referrer?: ?string;
    referrerPolicy?: ?ReferrerPolicyType;
}

type ResponseOptions = {
    status?: ?number;
    statusText?: ?string;
    headers?: ?HeadersInit
}

declare class Response {
    constructor(input?: string | URLSearchParams | FormData | Blob, init?: ResponseOptions): void;
    clone(): Response;
    static error(): Response;
    static redirect(url: string, status: number): Response;

    type: ResponseType;
    url: string;
    useFinalURL: boolean;
    ok: boolean;
    status: number;
    statusText: string;
    headers: Headers;

    // Body methods and attributes
    bodyUsed: boolean;

    arrayBuffer(): Promise<ArrayBuffer>;
    blob(): Promise<Blob>;
    formData(): Promise<FormData>;
    json(): Promise<any>;
    text(): Promise<string>;
}

declare class Request {
    constructor(input: string | Request, init?: RequestOptions): void;
    clone(): Request;

    url: string;

    cache: CacheType;
    credentials: CredentialsType;
    headers: Headers;
    integrity: string;
    method: MethodType;
    mode: ModeType;
    redirect: RedirectType;
    referrer: string;
    referrerPolicy: ReferrerPolicyType;

    // Body methods and attributes
    bodyUsed: boolean;

    arrayBuffer(): Promise<ArrayBuffer>;
    blob(): Promise<Blob>;
    formData(): Promise<FormData>;
    json(): Promise<any>;
    text(): Promise<string>;
}

declare function fetch(input: string | Request, init?: RequestOptions): Promise<Response>;


type TextEncoder$availableEncodings = 'utf-8' | 'utf8' | 'unicode-1-1-utf-8' | 'utf-16be' | 'utf-16' | 'utf-16le';

declare class TextEncoder {
  constructor(encoding?: TextEncoder$availableEncodings): TextEncoder;
  encode(buffer: string, options?: {
    stream: bool,
  }): Uint8Array;
  encoding: TextEncoder$availableEncodings;
}

type TextDecoder$availableEncodings =
  | '866'
  | 'ansi_x3.4-1968'
  | 'arabic'
  | 'ascii'
  | 'asmo-708'
  | 'big5-hkscs'
  | 'big5'
  | 'chinese'
  | 'cn-big5'
  | 'cp1250'
  | 'cp1251'
  | 'cp1252'
  | 'cp1253'
  | 'cp1254'
  | 'cp1255'
  | 'cp1256'
  | 'cp1257'
  | 'cp1258'
  | 'cp819'
  | 'cp866'
  | 'csbig5'
  | 'cseuckr'
  | 'cseucpkdfmtjapanese'
  | 'csgb2312'
  | 'csibm866'
  | 'csiso2022jp'
  | 'csiso2022kr'
  | 'csiso58gb231280'
  | 'csiso88596e'
  | 'csiso88596i'
  | 'csiso88598e'
  | 'csiso88598i'
  | 'csisolatin1'
  | 'csisolatin2'
  | 'csisolatin3'
  | 'csisolatin4'
  | 'csisolatin5'
  | 'csisolatin6'
  | 'csisolatin9'
  | 'csisolatinarabic'
  | 'csisolatincyrillic'
  | 'csisolatingreek'
  | 'csisolatinhebrew'
  | 'cskoi8r'
  | 'csksc56011987'
  | 'csmacintosh'
  | 'csshiftjis'
  | 'cyrillic'
  | 'dos-874'
  | 'ecma-114'
  | 'ecma-118'
  | 'elot_928'
  | 'euc-jp'
  | 'euc-kr'
  | 'gb_2312-80'
  | 'gb_2312'
  | 'gb18030'
  | 'gb2312'
  | 'gbk'
  | 'greek'
  | 'greek8'
  | 'hebrew'
  | 'hz-gb-2312'
  | 'ibm819'
  | 'ibm866'
  | 'iso_8859-1:1987'
  | 'iso_8859-1'
  | 'iso_8859-2:1987'
  | 'iso_8859-2'
  | 'iso_8859-3:1988'
  | 'iso_8859-3'
  | 'iso_8859-4:1988'
  | 'iso_8859-4'
  | 'iso_8859-5:1988'
  | 'iso_8859-5'
  | 'iso_8859-6:1987'
  | 'iso_8859-6'
  | 'iso_8859-7:1987'
  | 'iso_8859-7'
  | 'iso_8859-8:1988'
  | 'iso_8859-8'
  | 'iso_8859-9:1989'
  | 'iso_8859-9'
  | 'iso-2022-cn-ext'
  | 'iso-2022-cn'
  | 'iso-2022-jp'
  | 'iso-2022-kr'
  | 'iso-8859-1'
  | 'iso-8859-10'
  | 'iso-8859-11'
  | 'iso-8859-13'
  | 'iso-8859-14'
  | 'iso-8859-15'
  | 'iso-8859-16'
  | 'iso-8859-2'
  | 'iso-8859-3'
  | 'iso-8859-4'
  | 'iso-8859-5'
  | 'iso-8859-6-e'
  | 'iso-8859-6-i'
  | 'iso-8859-6'
  | 'iso-8859-7'
  | 'iso-8859-8-e'
  | 'iso-8859-8-i'
  | 'iso-8859-8'
  | 'iso-8859-9'
  | 'iso-ir-100'
  | 'iso-ir-101'
  | 'iso-ir-109'
  | 'iso-ir-110'
  | 'iso-ir-126'
  | 'iso-ir-127'
  | 'iso-ir-138'
  | 'iso-ir-144'
  | 'iso-ir-148'
  | 'iso-ir-149'
  | 'iso-ir-157'
  | 'iso-ir-58'
  | 'iso8859-1'
  | 'iso8859-10'
  | 'iso8859-11'
  | 'iso8859-13'
  | 'iso8859-14'
  | 'iso8859-15'
  | 'iso8859-2'
  | 'iso8859-3'
  | 'iso8859-4'
  | 'iso8859-6'
  | 'iso8859-7'
  | 'iso8859-8'
  | 'iso8859-9'
  | 'iso88591'
  | 'iso885910'
  | 'iso885911'
  | 'iso885913'
  | 'iso885914'
  | 'iso885915'
  | 'iso88592'
  | 'iso88593'
  | 'iso88594'
  | 'iso88595'
  | 'iso88596'
  | 'iso88597'
  | 'iso88598'
  | 'iso88599'
  | 'koi'
  | 'koi8_r'
  | 'koi8-r'
  | 'koi8-u'
  | 'koi8'
  | 'korean'
  | 'ks_c_5601-1987'
  | 'ks_c_5601-1989'
  | 'ksc_5601'
  | 'ksc5601'
  | 'l1'
  | 'l2'
  | 'l3'
  | 'l4'
  | 'l5'
  | 'l6'
  | 'l9'
  | 'latin1'
  | 'latin2'
  | 'latin3'
  | 'latin4'
  | 'latin5'
  | 'latin6'
  | 'latin9'
  | 'logical'
  | 'mac'
  | 'macintosh'
  | 'ms_kanji'
  | 'shift_jis'
  | 'shift-jis'
  | 'sjis'
  | 'sun_eu_greek'
  | 'tis-620'
  | 'unicode-1-1-utf-8'
  | 'us-ascii'
  | 'utf-16'
  | 'utf-16be'
  | 'utf-16le'
  | 'utf-8'
  | 'utf8'
  | 'visual'
  | 'windows-1250'
  | 'windows-1251'
  | 'windows-1252'
  | 'windows-1253'
  | 'windows-1254'
  | 'windows-1255'
  | 'windows-1256'
  | 'windows-1257'
  | 'windows-1258'
  | 'windows-31j'
  | 'windows-874'
  | 'windows-949'
  | 'x-cp1250'
  | 'x-cp1251'
  | 'x-cp1252'
  | 'x-cp1253'
  | 'x-cp1254'
  | 'x-cp1255'
  | 'x-cp1256'
  | 'x-cp1257'
  | 'x-cp1258'
  | 'x-euc-jp'
  | 'x-gbk'
  | 'x-mac-cyrillic'
  | 'x-mac-roman'
  | 'x-mac-ukrainian'
  | 'x-sjis'
  | 'x-user-defined'
  | 'x-x-big5';


declare class TextDecoder {
  constructor(encoding?: TextDecoder$availableEncodings, options?: { fatal: bool }): TextDecoder;
  encoding: TextDecoder$availableEncodings;
  fatal: bool;
  ignoreBOM: bool;
  decode(buffer?: $ArrayBufferView, options?: { stream: bool }): string;
}

declare class MessagePort extends EventTarget {
  postMessage(message: any, transfer?: Iterable<Object>): void;
  start(): void;
  close(): void;

  onmessage: (ev: Event) => any;
}

declare class MessageChannel {
  port1: MessagePort;
  port2: MessagePort;
}
