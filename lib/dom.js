/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */
/* Files */

declare class Blob {
    constructor(blobParts?: Array<any>, options?: {
        type?: string;
        endings?: string;
    }): void;
    type: string;
    size: number;
    slice(start?: number, end?: number, contentType?: string): Blob;
}

declare class FileReader extends BaseReader {
    error: DOMError;
    readAsArrayBuffer(blob: Blob): void;
    readAsDataURL(blob: Blob): void;
    readAsText(blob: Blob, encoding?: string): void;
}

declare class BaseReader extends EventTarget { // extension
    onprogress: (ev: any) => any;
    readyState: number;
    onabort: (ev: any) => any;
    onloadend: (ev: any) => any;
    onerror: (ev: any) => any;
    onload: (ev: any) => any;
    onloadstart: (ev: any) => any;
    result: any;
    abort(): void;
    LOADING: number;
    EMPTY: number;
    DONE: number;
}

declare class File extends Blob {
    lastModifiedDate: any;
    name: string;
}

declare class FileList {
    length: number;
    item(index: number): File;
    [index: number]: File;
}

/* DOM */

declare class DOMError {
    name: string;
}

declare class EventTarget {
    removeEventListener(type: string, listener: (evt: any) => void, useCapture?: boolean): void;
    addEventListener(type: string, listener: (evt: any) => void, useCapture?: boolean): void;
    dispatchEvent(evt: Event): boolean;
}

declare class Event {
    timeStamp: number;
    defaultPrevented: boolean;
    isTrusted: boolean;
    currentTarget: EventTarget;
    cancelBubble: boolean;
    target: EventTarget;
    eventPhase: number;
    cancelable: boolean;
    type: string;
    srcElement: Element;
    bubbles: boolean;
    initEvent(eventTypeArg: string, canBubbleArg: boolean, cancelableArg: boolean): void;
    stopPropagation(): void;
    stopImmediatePropagation(): void;
    preventDefault(): void;
    CAPTURING_PHASE: number;
    AT_TARGET: number;
    BUBBLING_PHASE: number;
}

declare class ProgressEvent extends Event {
    lengthComputable: boolean;
    loaded: number;
    total: number;
    initProgressEvent(
      typeArg: string,
      canBubbleArg: boolean,
      cancelableArg: boolean,
      lengthComputableArg: boolean,
      loadedArg: number,
      totalArg: number): void;
}

// used for websockets, for example. See:
// http://www.w3.org/TR/2011/WD-websockets-20110419/
// and
// http://www.w3.org/TR/2008/WD-html5-20080610/comms.html
declare class MessageEvent {
    data: string;
    origin: string;
    lastEventId: string;
    source: Document;
}

// TODO: *Event

declare class Node extends EventTarget {
    appendChild(newChild: Node): Node;
    attributes: NamedNodeMap;
    baseURI: ?string;
    childNodes: NodeList<Node>;
    cloneNode(deep?: boolean): Node;
    compareDocumentPosition(other: Node): number;
    contains(other: ?Node): boolean;
    firstChild: Node;
    hasAttributes(): boolean;
    hasChildNodes(): boolean;
    insertBefore(newChild: Node, refChild?: Node): Node;
    isDefaultNamespace(namespaceURI: string): boolean;
    isEqualNode(arg: Node): boolean;
    isSameNode(other: Node): boolean;
    isSupported(feature: string, version: string): boolean;
    lastChild: Node;
    localName: string;
    lookupNamespaceURI(prefix: string): string;
    lookupPrefix(namespaceURI: string): string;
    namespaceURI: string;
    nextSibling: Node;
    nodeName: string;
    nodeType: number;
    nodeValue: string;
    normalize(): void;
    ownerDocument: Document;
    parentElement: ?Element;
    parentNode: Node;
    prefix: string;
    previousSibling: Node;
    removeChild(oldChild: Node): Node;
    replaceChild(newChild: Node, oldChild: Node): Node;
    textContent: string;
    static ATTRIBUTE_NODE: number;
    static CDATA_SECTION_NODE: number;
    static COMMENT_NODE: number;
    static DOCUMENT_FRAGMENT_NODE: number;
    static DOCUMENT_NODE: number;
    static DOCUMENT_POSITION_CONTAINED_BY: number;
    static DOCUMENT_POSITION_CONTAINS: number;
    static DOCUMENT_POSITION_DISCONNECTED: number;
    static DOCUMENT_POSITION_FOLLOWING: number;
    static DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC: number;
    static DOCUMENT_POSITION_PRECEDING: number;
    static DOCUMENT_TYPE_NODE: number;
    static ELEMENT_NODE: number;
    static ENTITY_NODE: number;
    static ENTITY_REFERENCE_NODE: number;
    static NOTATION_NODE: number;
    static PROCESSING_INSTRUCTION_NODE: number;
    static TEXT_NODE: number;
}

declare class NodeList<T> {
    length: number;
    item(index: number): T;
    [index: number]: T;
}

declare class NamedNodeMap {
    length: number;
    removeNamedItemNS(namespaceURI: string, localName: string): Attr;
    item(index: number): Attr;
    [index: number]: Attr;
    removeNamedItem(name: string): Attr;
    getNamedItem(name: string): Attr;
    setNamedItem(arg: Attr): Attr;
    getNamedItemNS(namespaceURI: string, localName: string): Attr;
    setNamedItemNS(arg: Attr): Attr;
}

declare class Attr extends Node {
    isId: boolean;
    specified: boolean;
    ownerElement: Element;
    value: string;
    name: string;
}

declare class HTMLCollection {
    length: number;
    item(nameOrIndex?: any, optionalIndex?: any): Element;
    namedItem(name: string): Element;
    [index: number]: Element;
}

declare class Document extends Node {
    documentElement: HTMLElement;
    implementation: DOMImplementation;
    scripts: HTMLCollection;
    styleSheets: StyleSheetList;
    title: string;
    embeds: HTMLCollection;
    forms: HTMLCollection;
    inputEncoding: string;
    links: HTMLCollection;
    URL: string;
    head: HTMLElement;
    cookie: string;
    documentMode: number;
    characterSet: string;
    anchors: HTMLCollection;
    readyState: string;
    referrer: string;
    doctype: DocumentType;
    applets: HTMLCollection;
    body: HTMLElement;
    domain: string;
    media: string;
    images: HTMLCollection;
    lastModified: string;
    getElementById(elementId: string): HTMLElement;
    adoptNode(source: Node): Node;
    createCDATASection(data: string): Text;
    write(...content: Array<string>): void;
    createElement(tagName: 'a'): HTMLAnchorElement;
    createElement(tagName: 'button'): HTMLButtonElement;
    createElement(tagName: 'canvas'): HTMLCanvasElement;
    createElement(tagName: 'form'): HTMLFormElement;
    createElement(tagName: 'img'): HTMLImageElement;
    createElement(tagName: 'input'): HTMLInputElement;
    createElement(tagName: 'media'): HTMLMediaElement;
    createElement(tagName: 'script'): HTMLScriptElement;
    createElement(tagName: 'select'): HTMLSelectElement;
    createElement(tagName: 'style'): HTMLStyleElement;
    createElement(tagName: 'textarea'): HTMLTextAreaElement;
    createElement(tagName: 'video'): HTMLVideoElement;
    createElement(tagName: string): HTMLElement;
    writeln(...content: Array<string>): void;
    getElementsByTagNameNS(namespaceURI: string, localName: string): HTMLCollection;
    createElementNS(namespaceURI: string, qualifiedName: string): Element;
    open(url?: string, name?: string, features?: string, replace?: boolean): any;
    createAttributeNS(namespaceURI: string, qualifiedName: string): Attr;
    close(): void;
    getElementsByClassName(classNames: string): HTMLCollection;
    importNode(importedNode: Node, deep: boolean): Node;
    createComment(data: string): Comment;
    getElementsByTagName(name: string): HTMLCollection;
    createDocumentFragment(): Node;
    getElementsByName(elementName: string): HTMLCollection;
    createAttribute(name: string): Attr;
    createTextNode(data: string): Text;
    execCommand(cmdID: string, showUI?: boolean, value?: any): boolean;
    xmlEncoding: string;
    xmlStandalone: boolean;
    xmlVersion: string;

    // extension
    location: Location;
    createEvent(eventInterface: string): Event;
    createRange(): Range;
    elementFromPoint(x: number, y: number): HTMLElement;
    defaultView: any;
    compatMode: string;
    activeElement: HTMLElement;
    hidden: boolean;

    // from ParentNode interface
    childElementCount: number;
    children: HTMLCollection;
    firstElementChild: ?Element;
    lastElementChild: ?Element;
    querySelector(selector: string): HTMLElement;
    querySelectorAll(selector: string): NodeList<HTMLElement>;
}

declare class DocumentFragment extends Node {
    // from ParentNode interface
    childElementCount: number;
    children: HTMLCollection;
    firstElementChild: ?Element;
    lastElementChild: ?Element;
    querySelector(selector: string): HTMLElement;
    querySelectorAll(selector: string): NodeList<HTMLElement>;
}

declare class Range { // extension
    startOffset: number;
    collapsed: boolean;
    endOffset: number;
    startContainer: Node;
    endContainer: Node;
    commonAncestorContainer: Node;
    setStart(refNode: Node, offset: number): void;
    setEndBefore(refNode: Node): void;
    setStartBefore(refNode: Node): void;
    selectNode(refNode: Node): void;
    detach(): void;
    getBoundingClientRect(): ClientRect;
    toString(): string;
    compareBoundaryPoints(how: number, sourceRange: Range): number;
    insertNode(newNode: Node): void;
    collapse(toStart: boolean): void;
    selectNodeContents(refNode: Node): void;
    cloneContents(): Node;
    setEnd(refNode: Node, offset: number): void;
    cloneRange(): Range;
    getClientRects(): ClientRectList;
    surroundContents(newParent: Node): void;
    deleteContents(): void;
    setStartAfter(refNode: Node): void;
    extractContents(): Node;
    setEndAfter(refNode: Node): void;
    createContextualFragment(fragment: string): Node;
    END_TO_END: number;
    START_TO_START: number;
    START_TO_END: number;
    END_TO_START: number;
}

declare var document: Document;

// TODO: HTMLDocument

declare class DOMTokenList {
  length: number;
  item(index: number): string;
  contains(token: string): boolean;
  add(token: string): void;
  remove(token: string): void;
  toggle(token: string): boolean;
}

declare class Element extends Node {
    classList: DOMTokenList;
    className: string;
    clientHeight: number;
    clientLeft: number;
    clientTop: number;
    clientWidth: number;
    id: string;
    innerHTML: string;
    nextElementSibling: Element;
    outerHTML: string;
    previousElementSibling: Element;
    scrollHeight: number;
    scrollLeft: number;
    scrollTop: number;
    scrollWidth: number;
    tagName: string;

    getAttribute(name?: string): string;
    getAttributeNS(namespaceURI: string, localName: string): string;
    getAttributeNode(name: string): Attr;
    getAttributeNodeNS(namespaceURI: string, localName: string): Attr;
    getBoundingClientRect(): ClientRect;
    getElementsByClassName(names: string): HTMLCollection;
    getElementsByTagName(name: string): HTMLCollection;
    getElementsByTagNameNS(namespaceURI: string, localName: string): NodeList<HTMLElement>;
    hasAttribute(name: string): boolean;
    hasAttributeNS(namespaceURI: string, localName: string): boolean;
    insertAdjacentHTML(position: string, text: string): void;
    removeAttribute(name?: string): void;
    removeAttributeNS(namespaceURI: string, localName: string): void;
    removeAttributeNode(oldAttr: Attr): Attr;
    scrollIntoView(top?: boolean): void;
    setAttribute(name?: string, value?: string): void;
    setAttributeNS(namespaceURI: string, qualifiedName: string, value: string): void;
    setAttributeNode(newAttr: Attr): Attr;
    setAttributeNodeNS(newAttr: Attr): Attr;

    // from ParentNode interface
    childElementCount: number;
    children: HTMLCollection;
    firstElementChild: ?Element;
    lastElementChild: ?Element;
    querySelector(selector: string): HTMLElement;
    querySelectorAll(selector: string): NodeList<HTMLElement>;
}

declare class HTMLElement extends Element {
    accessKey: string;
    accessKeyLabel: string;
    blur(): void;
    className: string;
    click(): void;
    contentEditable: string;
    dataset: {[key:string]: string};
    dir: string;
    focus(): void;
    getBoundingClientRect(): ClientRect;
    hidden: boolean;
    id: string;
    innerHTML: string;
    isContentEditable: boolean;
    lang: string;
    offsetHeight: number;
    offsetLeft: number;
    offsetParent: Element;
    offsetTop: number;
    offsetWidth: number;
    spellcheck: boolean;
    tabIndex: number;
    title: string;
    translate: boolean;

    // extension
    onerror: (ev: any) => void;
    onload: (ev: any) => void;
    onreadystatechange: (ev: any) => any;
    scrollIntoView(top?: boolean): void;
    spellcheck: boolean;
    style: CSSStyleDeclaration;
}

declare class HTMLBaseElement extends HTMLElement {
    href: string;
    target: string;
}

declare class CanvasGradient {
    addColorStop(offset: number, color: string): void;
}

declare class CanvasPattern {
}

declare class ImageBitmap {
    width: number;
    height: number;
}

type CanvasFillRule = string;

type CanvasImageSource = HTMLImageElement | HTMLVideoElement | HTMLCanvasElement | CanvasRenderingContext2D | ImageBitmap;

declare class HitRegionOptions {
    path?: Path2D,
    fillRule?: CanvasFillRule,
    id?: string,
    parentID?: string;
    cursor?: string;
    control?: Element;
    label: ?string;
    role: ?string;
};

declare class CanvasDrawingStyles {
    lineWidth: number;
    lineCap: string;
    lineJoin: string;
    miterLimit: number;

    // dashed lines
    setLineDash(segments: Array<number>): void;
    getLineDash(): Array<number>;
    lineDashOffset: number;

    // text
    font: string;
    textAlign: string;
    textBaseline: string;
    direction: string;
};

declare class SVGMatrix {
    getComponent(index: number): number;
    mMultiply(secondMatrix: SVGMatrix): SVGMatrix;
    inverse(): SVGMatrix;
    mTranslate(x: number, y: number): SVGMatrix;
    mScale(scaleFactor: number): SVGMatrix;
    mRotate(angle: number): SVGMatrix;
};

declare class TextMetrics {
    // x-direction
    width: number;
    actualBoundingBoxLeft: number;
    actualBoundingBoxRight: number;

    // y-direction
    fontBoundingBoxAscent: number;
    fontBoundingBoxDescent: number;
    actualBoundingBoxAscent: number;
    actualBoundingBoxDescent: number;
    emHeightAscent: number;
    emHeightDescent: number;
    hangingBaseline: number;
    alphabeticBaseline: number;
    ideographicBaseline: number;
};

declare class Path2D {
    addPath(path: Path2D, transformation?: ?SVGMatrix): void;
    addPathByStrokingPath(path: Path2D, styles: CanvasDrawingStyles, transformation?: ?SVGMatrix): void;
    addText(text: string, styles: CanvasDrawingStyles, transformation: ?SVGMatrix, x: number, y: number, maxWidth?: number): void;
    addPathByStrokingText(text: string, styles: CanvasDrawingStyles, transformation: ?SVGMatrix, x: number, y: number, maxWidth?: number): void;
    addText(text: string, styles: CanvasDrawingStyles, transformation: ?SVGMatrix, path: Path2D, maxWidth?: number): void;
    addPathByStrokingText(text: string, styles: CanvasDrawingStyles, transformation: ?SVGMatrix, path: Path2D, maxWidth?: number): void;
};

declare class ImageData {
    width: number;
    height: number;
    data: Array<number>;
};

declare class CanvasRenderingContext2D {
    canvas: HTMLCanvasElement;

    // canvas dimensions
    width: number;
    height: number;

    // for contexts that aren't directly fixed to a specific canvas
    commit(): void;

    // state
    save(): void;
    restore(): void;

    // transformations
    currentTransform: SVGMatrix;
    scale(x: number, y: number): void;
    rotate(angle: number): void;
    translate(x: number, y: number): void;
    transform(a: number, b: number, c: number, d: number, e: number, f: number): void;
    setTransform(a: number, b: number, c: number, d: number, e: number, f: number): void;
    resetTransform(): void;

    // compositing
    globalAlpha: number;
    globalCompositeOperation: string;

    // image smoothing
    imageSmoothingEnabled: boolean;

    // colours and styles
    strokeStyle: string | CanvasGradient | CanvasPattern;
    fillStyle: string | CanvasGradient | CanvasPattern;
    createLinearGradient(x0: number, y0: number, x1: number, y1: number): CanvasGradient;
    createRadialGradient(x0: number, y0: number, r0: number, x1: number, y1: number, r1: number): CanvasGradient;
    createPattern(image: CanvasImageSource, repetition: ?string): CanvasPattern;

    // shadows
    shadowOffsetX: number;
    shadowOffsetY: number;
    shadowBlur: number;
    shadowColor: string;

    // rects
    clearRect(x: number, y: number, w: number, h: number): void;
    fillRect(x: number, y: number, w: number, h: number): void;
    strokeRect(x: number, y: number, w: number, h: number): void;

    // path API
    beginPath(): void;
    fill(fillRule?: CanvasFillRule): void;
    fill(path: Path2D, fillRule?: CanvasFillRule): void;
    stroke(): void;
    stroke(path: Path2D): void;
    drawFocusIfNeeded(element: Element): void;
    drawFocusIfNeeded(path: Path2D, element: Element): void;
    scrollPathIntoView(): void;
    scrollPathIntoView(path: Path2D): void;
    clip(fillRule?: CanvasFillRule): void;
    clip(path: Path2D, fillRule?: CanvasFillRule): void;
    resetClip(): void;
    isPointInPath(x: number, y: number, fillRule?: CanvasFillRule): boolean;
    isPointInPath(path: Path2D, x: number, y: number, fillRule?: CanvasFillRule): boolean;
    isPointInStroke(x: number, y: number): boolean;
    isPointInStroke(path: Path2D, x: number, y: number): boolean;

    // text (see also the CanvasDrawingStyles interface)
    fillText(text: string, x: number, y: number, maxWidth?: number): void;
    strokeText(text: string, x: number, y: number, maxWidth?: number): void;
    measureText(text: string): TextMetrics;

    // drawing images
    drawImage(image: CanvasImageSource, dx: number, dy: number): void;
    drawImage(image: CanvasImageSource, dx: number, dy: number, dw: number, dh: number): void;
    drawImage(image: CanvasImageSource, sx: number, sy: number, sw: number, sh: number, dx: number, dy: number, dw: number, dh: number): void;

    // hit regions
    addHitRegion(options?: HitRegionOptions): void;
    removeHitRegion(id: string): void;
    clearHitRegions(): void;

    // pixel manipulation
    createImageData(sw: number, sh: number): ImageData;
    createImageData(imagedata: ImageData): ImageData;
    getImageData(sx: number, sy: number, sw: number, sh: number): ImageData;
    putImageData(imagedata: ImageData, dx: number, dy: number): void;
    putImageData(imagedata: ImageData, dx: number, dy: number, dirtyX: number, dirtyY: number, dirtyWidth: number, dirtyHeight: number): void;

    // CanvasDrawingStyles
    // line caps/joins
    lineWidth: number;
    lineCap: string;
    lineJoin: string;
    miterLimit: number;

    // dashed lines
    setLineDash(segments: Array<number>): void;
    getLineDash(): Array<number>;
    lineDashOffset: number;

    // text
    font: string;
    textAlign: string;
    textBaseline: string;
    direction: string;

    // CanvasPathMethods
    // shared path API methods
    closePath(): void;
    moveTo(x: number, y: number): void;
    lineTo(x: number, y: number): void;
    quadraticCurveTo(cpx: number, cpy: number, x: number, y: number): void;
    bezierCurveTo(cp1x: number, cp1y: number, cp2x: number, cp2y: number, x: number, y: number): void;
    arcTo(x1: number, y1: number, x2: number, y2: number, radius: number): void;
    arcTo(x1: number, y1: number, x2: number, y2: number, radiusX: number, radiusY: number, rotation: number): void;
    rect(x: number, y: number, w: number, h: number): void;
    arc(x: number, y: number, radius: number, startAngle: number, endAngle: number, anticlockwise?: boolean): void;
    ellipse(x: number, y: number, radiusX: number, radiusY: number, rotation: number, startAngle: number, endAngle: number, anticlockwise?: boolean): void;
}

declare class WebGLRenderingContext {}

// http://www.w3.org/TR/html5/scripting-1.html#renderingcontext
type RenderingContext = CanvasRenderingContext2D | WebGLRenderingContext;

// http://www.w3.org/TR/html5/scripting-1.html#htmlcanvaselement
declare class HTMLCanvasElement extends HTMLElement {
    width: number;
    height: number;
    getContext(contextId: string, ...args: any): ?RenderingContext;
    toDataURL(type?: string, ...args: any): string;
    toBlob(callback: (v: File) => void, type?: string, ...args: any): void;
}

declare class HTMLCollection {
    [name: number]: Element;
    length: number;
    item(nameOrIndex?: any, optionalIndex?: any): Element;
    namedItem(name: string): Element;
}

declare class HTMLFormElement extends HTMLElement {
    [name: string]: any;
    acceptCharset: string;
    action: string;
    elements: HTMLCollection;
    encoding: string;
    enctype: string;
    length: number;
    method: string;
    name: string;
    target: string;

    item(name?: any, index?: any): any;
    namedItem(name: string): any;
    reset(): void;
    submit(): void;
}

declare class HTMLImageElement extends HTMLElement {
    alt: string;
    complete: boolean; // readonly
    crossOrigin: string;
    currentSrc: string; // readonly
    height: number;
    isMap: boolean;
    naturalHeight: number; // readonly
    naturalWidth: number; // readonly
    sizes: string;
    src: string;
    srcset: string;
    useMap: string;
    width: number;
}

declare class Image extends HTMLImageElement {
    constructor(width?: number, height?: number): void;
}

declare class MediaError {
    MEDIA_ERR_ABORTED: number;
    MEDIA_ERR_NETWORK: number;
    MEDIA_ERR_DECODE: number;
    MEDIA_ERR_SRC_NOT_SUPPORTED: number;
    code: number;
}

declare class TimeRanges {
    length: number;
    start(index: number): number;
    end(index: number): number;
}

declare class AudioTrack {
    id: string;
    kind: string;
    label: string;
    language: string;
    enabled: boolean;
}

declare class AudioTrackList extends EventTarget {
    length: number;
    [index: number]: AudioTrack;

    getTrackById(id: string): ?AudioTrack;

    onchange: (ev: any) => any;
    onaddtrack: (ev: any) => any;
    onremovetrack: (ev: any) => any;
}

declare class VideoTrack {
    id: string;
    kind: string;
    label: string;
    language: string;
    selected: boolean;
}

declare class VideoTrackList extends EventTarget {
    length: number;
    [index: number]: VideoTrack;
    getTrackById(id: string): ?VideoTrack;
    selectedIndex: number;

    onchange: (ev: any) => any;
    onaddtrack: (ev: any) => any;
    onremovetrack: (ev: any) => any;
}

declare class TextTrackCue extends EventTarget {
    constructor(startTime: number, endTime: number, text: string): void;

    track: TextTrack;
    id: string;
    startTime: number;
    endTime: number;
    pauseOnExit: boolean;
    vertical: string;
    snapToLines: boolean;
    lines: number;
    position: number;
    size: number;
    align: string;
    text: string;

    getCueAsHTML(): Node;
    onenter: (ev: any) => any;
    onexit: (ev: any) => any;
}

declare class TextTrackCueList {
    length: number;
    [index: number]: TextTrackCue;
    getCueById(id: string): ?TextTrackCue;
}

declare class TextTrack extends EventTarget {
    kind: string;
    label: string;
    language: string;

    mode: string;

    cues: TextTrackCueList;
    activeCues: TextTrackCueList;

    addCue(cue: TextTrackCue): void;
    removeCue(cue: TextTrackCue): void;

    oncuechange: (ev: any) => any;
}

declare class TextTrackList extends EventTarget {
    length: number;
    [index: number]: TextTrack;

    onaddtrack: (ev: any) => any;
    onremovetrack: (ev: any) => any;
}

declare class HTMLMediaElement extends HTMLElement {
    // error state
    error: ?MediaError;

    // network state
    src: string;
    srcObject: ?any;
    currentSrc: string;
    crossOrigin: string;
    NETWORK_EMPTY: number;
    NETWORK_IDLE: number;
    NETWORK_LOADING: number;
    NETWORK_NO_SOURCE: number;
    networkState: number;
    preload: string;
    buffered: TimeRanges;
    load(): void;
    canPlayType(type: string): string;

    // ready state
    HAVE_NOTHING: number;
    HAVE_METADATA: number;
    HAVE_CURRENT_DATA: number;
    HAVE_FUTURE_DATA: number;
    HAVE_ENOUGH_DATA: number;
    readyState: number;
    seeking: boolean;

    // playback state
    currentTime: number;
    duration: number;
    startDate: Date;
    paused: boolean;
    defaultPlaybackRate: number;
    playbackRate: number;
    played: TimeRanges;
    seekable: TimeRanges;
    ended: boolean;
    autoplay: boolean;
    loop: boolean;
    play(): void;
    pause(): void;
    fastSeek(): void;

    // media controller
    mediaGroup: string;
    controller: ?any;

    // controls
    controls: boolean;
    volume: number;
    muted: boolean;
    defaultMuted: boolean;

    // tracks
    audioTracks: AudioTrackList;
    videoTracks: VideoTrackList;
    textTracks: TextTrackList;
    addTextTrack(kind: string, label?: string, language?: string): TextTrack;
}

declare class HTMLVideoElement extends HTMLMediaElement {
    width: number;
    height: number;
    videoWidth: number;
    videoHeight: number;
    poster: string;
}

declare class HTMLInputElement extends HTMLElement {
    accept: string;
    align: string;
    alt: string;
    border: string;
    checked: boolean;
    complete: boolean;
    defaultChecked: boolean;
    defaultValue: string;
    disabled: boolean;
    dynsrc: string;
    files: FileList;
    form: HTMLFormElement;
    height: string;
    hspace: number;
    indeterminate: boolean;
    loop: number;
    lowsrc: string;
    maxLength: number;
    name: string;
    readOnly: boolean;
    selectionEnd: number;
    selectionStart: number;
    size: number;
    src: string;
    start: string;
    status: boolean;
    type: string;
    useMap: string;
    value: string;
    vrml: string;
    vspace: number;
    width: string;

    createTextRange(): TextRange;
    select(): void;
    setSelectionRange(start: number, end: number): void;
}

declare class HTMLButtonElement extends HTMLElement {
  disabled: boolean;
  form: HTMLFormElement ;
  name: string;
  type: string;
  value: string;
}

declare class HTMLTextAreaElement extends HTMLElement {
  cols: number;
  defaultValue: string;
  disabled: boolean;
  form: HTMLFormElement;
  name: string;
  readOnly: boolean;
  rows: number;
  type: string;
  value: string;

  select(): void;
}

declare class HTMLSelectElement extends HTMLElement {
  disabled: boolean;
  form: HTMLFormElement;
  length: number;
  multiple: boolean;
  name: string;
  options: HTMLOptionsCollection;
  selectedIndex: number;
  size: number;
  type: string;
  value: string;

  add(element: HTMLElement, before?: HTMLElement): void;
  remove(index: number): void;
}

declare class HTMLOptionsCollection {
  length: number;
  item(index: number): Node;
  namedItem(name: string): Node;
};

declare class HTMLAnchorElement extends HTMLElement {
    charset: string;
    coords: string;
    download: string;
    hash: string;
    host: string;
    hostname: string;
    href: string;
    hreflang: string;
    media: string;
    name: string;
    origin: string;
    password: string;
    pathname: string;
    port: string;
    protocol: string;
    rel: string;
    rev: string;
    search: string;
    shape: string;
    text: string;
    type: string;
    username: string;
}

declare class HTMLScriptElement extends HTMLElement {
    async: boolean;
    charset: string;
    crossOrigin?: string;
    defer: boolean;
    src: string;
    text: string;
    type: string;
}

declare class HTMLStyleElement extends HTMLElement {
    disabled: boolean;
    media: string;
    scoped: boolean;
    sheet: ?StyleSheet;
    type: string;
}

declare class TextRange {
    boundingLeft: number;
    htmlText: string;
    offsetLeft: number;
    boundingWidth: number;
    boundingHeight: number;
    boundingTop: number;
    text: string;
    offsetTop: number;
    moveToPoint(x: number, y: number): void;
    queryCommandValue(cmdID: string): any;
    getBookmark(): string;
    move(unit: string, count?: number): number;
    queryCommandIndeterm(cmdID: string): boolean;
    scrollIntoView(fStart?: boolean): void;
    findText(string: string, count?: number, flags?: number): boolean;
    execCommand(cmdID: string, showUI?: boolean, value?: any): boolean;
    getBoundingClientRect(): ClientRect;
    moveToBookmark(bookmark: string): boolean;
    isEqual(range: TextRange): boolean;
    duplicate(): TextRange;
    collapse(start?: boolean): void;
    queryCommandText(cmdID: string): string;
    select(): void;
    pasteHTML(html: string): void;
    inRange(range: TextRange): boolean;
    moveEnd(unit: string, count?: number): number;
    getClientRects(): ClientRectList;
    moveStart(unit: string, count?: number): number;
    parentElement(): Element;
    queryCommandState(cmdID: string): boolean;
    compareEndPoints(how: string, sourceRange: TextRange): number;
    execCommandShowHelp(cmdID: string): boolean;
    moveToElementText(element: Element): void;
    expand(Unit: string): boolean;
    queryCommandSupported(cmdID: string): boolean;
    setEndPoint(how: string, SourceRange: TextRange): void;
    queryCommandEnabled(cmdID: string): boolean;
}

declare class ClientRect { // extension
    left: number;
    width: number;
    right: number;
    top: number;
    bottom: number;
    height: number;
}

declare class ClientRectList { // extension
    length: number;
    item(index: number): ClientRect;
    [index: number]: ClientRect;
}

// TODO: HTML*Element

declare class DOMImplementation {
    createDocumentType(qualifiedName: string, publicId: string, systemId: string): DocumentType;
    createDocument(namespaceURI: string, qualifiedName: string, doctype: DocumentType): Document;
    hasFeature(feature: string, version?: string): boolean;

    // non-standard
    createHTMLDocument(title?: string): Document;
}

declare class DocumentType extends Node {
    name: string;
    notations: NamedNodeMap;
    systemId: string;
    internalSubset: string;
    entities: NamedNodeMap;
    publicId: string;
}

declare class CharacterData extends Node {
    length: number;
    data: string;
    deleteData(offset: number, count: number): void;
    replaceData(offset: number, count: number, arg: string): void;
    appendData(arg: string): void;
    insertData(offset: number, arg: string): void;
    substringData(offset: number, count: number): string;
}

declare class Text extends CharacterData {
    wholeText: string;
    splitText(offset: number): Text;
    replaceWholeText(content: string): Text;
}

declare class Comment extends CharacterData {
    text: string;
}

declare class URL {
  static createObjectURL(blob: Blob): string;
  static createObjectURL(mediaSource: MediaSource): string;
  static createFor(blob: Blob): string;
  static revokeObjectURL(url: string): void;
}

declare class MediaSource extends EventTarget {
    sourceBuffers: SourceBufferList;
    activeSourceBuffers: SourceBufferList;
    readyState: "closed" | "opened" | "ended";
    duration: number;
    addSourceBuffer(type: string): SourceBuffer;
    removeSourceBuffer(sourceBuffer: SourceBuffer): void;
    endOfStream(error?: string): void;
    static isTypeSupported(type: string): bool;
}

declare class SourceBuffer extends EventTarget {
  mode: "segments" | "sequence";
  updating: bool;
  buffered: TimeRanges;
  timestampOffset: number;
  audioTracks: AudioTrackList;
  videoTracks: VideoTrackList;
  textTracks: TextTrackList;
  appendWindowStart: number;
  appendWindowEnd: number;

  appendBuffer(data: ArrayBuffer): void;
  appendBuffer(ata: ArrayBufferView): void;
  // TODO: Add ReadableStream
  // appendStream(stream: ReadableStream, maxSize?: number): void;
  abort(): void;
  remove(start: number, end: number): void;

  trackDefaults: TrackDefaultList;
}

declare class SourceBufferList extends EventTarget {
  [index: number]: SourceBuffer;
  length: number;
}

declare class Storage {
    length: number;
    getItem(key: string): ?string;
    setItem(key: string, data: string): void;
    clear(): void;
    removeItem(key: string): void;
    key(index: number): ?string;
    [name: string]: ?string;
}

declare class TrackDefaultList {
  [index: number]: TrackDefault;
  length: number;
}

declare class TrackDefault {
  type: "audio" | "video" | "text";
  byteStreamTrackID: string;
  language: string;
  label: string;
  kinds: Array<string>;
}

/* window */

declare function alert(message?: any): void;
declare function prompt(message?: any, value?: any): string;
declare function close(): void;
declare function confirm(message?: string): boolean;
declare var event: Event;
declare function getComputedStyle(elt: Element, pseudoElt?: string): any;
declare var localStorage: Storage;
declare function onfocus(ev: Event): any;
declare function onmessage(ev: Event): any;
declare function open(url?: string, target?: string, features?: string, replace?: boolean): any;
declare var parent: any;
declare function print(): void;
declare var self: any;
declare var sessionStorage: Storage;
declare var status: string;
declare var top: any;
