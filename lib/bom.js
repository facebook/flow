/**
 * Copyright (c) 2014, Facebook, Inc.
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
        angle: number;
        onchange: any;
        type: string;
    };
    pixelDepth: number;
    top: number;
    width: number;
    mozLockOrientation?: Function;
    mozUnlockOrientation?: Function;
}

declare var screen: Screen;
declare var window: any;

declare class Navigator {
    appCodeName: string;
    appName: string;
    appVersion: string;
    cookieEnabled: boolean;
    buildId: string;
    doNotTrack?: any;
    geolocation: Geolocation;
    mediaDevices?: Object;
    hardwareConcurrency: number;
    language: string;
    languages: Array<string>;
    maxTouchPoints: number;
    mimeTypes: Array<Object>;
    onLine: boolean;
    ospu: string;
    permissions: any;
    platform: string;
    plugins: Array<Object>;
    product: string;
    productSub: string;
    serviceWorker?: Object;
    userAgent: string;
    vendor: string;
    vendorSub: string;
    getBattery?: Function;
    getGamepads?: Function;
    webkitGetGamepads?: Function;
    mozGetGamepads?: Function;
    gamepads?: any,
    webkitGamepads?: any,
    mozGamepads?: any,
    getStorageUpdates?: Function;
    javaEnabled: Function;
    registerProtocolHandler: Function;
    requestMIDIAccess?: Function;
    requestMediaKeySystemAccess?: Function;
    sendBeacon?: Function;
    unregisterProtocolHandler: Function;
    vibrate?: Function;
    getUserMedia?: Function;
    webkitGetUserMedia?: Function;
    mozGetUserMedia?: Function;
    msGetUserMedia?: Function;
    taintEnabled?: Function;
}

declare var navigator: Navigator;

declare class History {
    length: number;
    state: any;
    back(distance?: any): void;
    forward(distance?: any): void;
    go(delta?: any): void;
    replaceState(statedata: any, title: string, url?: string): void;
    pushState(statedata: any, title: string, url?: string): void;
}

declare var history: History;

declare class Location {
    hash: string;
    origin: string;
    protocol: string;
    search: string;
    href: string;
    hostname: string;
    port: string;
    pathname: string;
    host: string;
    reload(flag?: boolean): void;
    replace(url: string): void;
    assign(url: string): void;
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
    oldValue: string;
    previousSibling: Node;
    addedNodes: NodeList;
    attributeName: string;
    removedNodes: NodeList;
    target: Node;
    nextSibling: Node;
    attributeNamespace: string;
    type: string;
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

declare class WebSocket extends EventTarget {
    constructor(url: string, protocols?: string | Array<string>): void;
    protocol: string;
    readyState: number;
    bufferedAmount: number;
    onopen: (ev: Event) => any;
    extensions: string;
    onmessage: (ev: MessageEvent) => any;
    onclose: (ev: Event) => any;
    onerror: (ev: Event) => any;
    binaryType: string;
    url: string;
    close(code?: number, reason?: string): void;
    send(data: any): void;
    OPEN: number;
    CLOSING: number;
    CONNECTING: number;
    CLOSED: number;
}

declare class Worker extends EventTarget {
    constructor(stringUrl: string): void;
    onerror: (ev: Event) => any;
    onmessage: (ev: Event) => any;
    postMessage(message: any, ports?: any): void;
    terminate(): void;
}

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
    ontimeout: (ev: Event) => any;
    statusText: string;
    onreadystatechange: (ev: Event) => any;
    timeout: number;
    onload: (ev: Event) => any;
    response: any;
    withCredentials: boolean;
    onprogress: (ev: ProgressEvent) => any;
    onabort: (ev: Event) => any;
    responseType: string;
    onloadend: (ev: Event) => any;
    upload: XMLHttpRequestEventTarget;
    onerror: (ev: Event) => any;
    onloadstart: (ev: Event) => any;
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

declare class PositionOptions {
    enableHighAccuracy: boolean;
    timeout: number;
    maximumAge: number;
}
