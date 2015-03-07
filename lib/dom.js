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
    nodeType: number;
    previousSibling: Node;
    localName: string;
    namespaceURI: string;
    textContent: string;
    parentNode: Node;
    nextSibling: Node;
    nodeValue: string;
    lastChild: Node;
    childNodes: NodeList<Node>;
    nodeName: string;
    ownerDocument: Document;
    attributes: NamedNodeMap;
    firstChild: Node;
    prefix: string;
    removeChild(oldChild: Node): Node;
    appendChild(newChild: Node): Node;
    isSupported(feature: string, version: string): boolean;
    isEqualNode(arg: Node): boolean;
    lookupPrefix(namespaceURI: string): string;
    isDefaultNamespace(namespaceURI: string): boolean;
    compareDocumentPosition(other: Node): number;
    normalize(): void;
    isSameNode(other: Node): boolean;
    hasAttributes(): boolean;
    lookupNamespaceURI(prefix: string): string;
    cloneNode(deep?: boolean): Node;
    hasChildNodes(): boolean;
    replaceChild(newChild: Node, oldChild: Node): Node;
    insertBefore(newChild: Node, refChild?: Node): Node;
    static ENTITY_REFERENCE_NODE: number;
    static ATTRIBUTE_NODE: number;
    static DOCUMENT_FRAGMENT_NODE: number;
    static TEXT_NODE: number;
    static ELEMENT_NODE: number;
    static COMMENT_NODE: number;
    static DOCUMENT_POSITION_DISCONNECTED: number;
    static DOCUMENT_POSITION_CONTAINED_BY: number;
    static DOCUMENT_POSITION_CONTAINS: number;
    static DOCUMENT_TYPE_NODE: number;
    static DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC: number;
    static DOCUMENT_NODE: number;
    static ENTITY_NODE: number;
    static PROCESSING_INSTRUCTION_NODE: number;
    static CDATA_SECTION_NODE: number;
    static NOTATION_NODE: number;
    static DOCUMENT_POSITION_FOLLOWING: number;
    static DOCUMENT_POSITION_PRECEDING: number;
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
    querySelectorAll(selectors: string): NodeList<HTMLElement>;
    querySelector(selectors: string): HTMLElement;
    createEvent(eventInterface: string): Event;
    createRange(): Range;
    elementFromPoint(x: number, y: number): HTMLElement;
    defaultView: any;
    compatMode: string;
    activeElement: HTMLElement;
    hidden: boolean;
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

declare class Element extends Node {
    scrollTop: number;
    clientLeft: number;
    scrollLeft: number;
    tagName: string;
    clientWidth: number;
    scrollWidth: number;
    clientHeight: number;
    clientTop: number;
    scrollHeight: number;
    getAttribute(name?: string): string;
    getElementsByTagNameNS(namespaceURI: string, localName: string): NodeList<HTMLElement>;
    hasAttributeNS(namespaceURI: string, localName: string): boolean;
    getAttributeNS(namespaceURI: string, localName: string): string;
    getAttributeNodeNS(namespaceURI: string, localName: string): Attr;
    setAttributeNodeNS(newAttr: Attr): Attr;
    hasAttribute(name: string): boolean;
    removeAttribute(name?: string): void;
    setAttributeNS(namespaceURI: string, qualifiedName: string, value: string): void;
    getAttributeNode(name: string): Attr;
    getElementsByTagName(name: string): HTMLCollection;
    setAttributeNode(newAttr: Attr): Attr;
    removeAttributeNode(oldAttr: Attr): Attr;
    setAttribute(name?: string, value?: string): void;
    removeAttributeNS(namespaceURI: string, localName: string): void;
    querySelector(selector: string): HTMLElement;
    querySelectorAll(selector: string): NodeList<HTMLElement>;
}

declare class HTMLElement extends Element {
    id: string;
    title: string;
    lang: string;
    dir: string;
    className: string;

    // extension
    getBoundingClientRect(): ClientRect;
    style: any;
    innerHTML: string;
    scrollIntoView(top?: boolean): void;
    onerror: (ev: any) => void;
    onload: (ev: any) => void;
    onreadystatechange: (ev: any) => any;
}

declare class HTMLBaseElement extends HTMLElement {
    href: string;
    target: string;
}

declare class CanvasRenderingContext2D {}

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
    src: string;
    usemap: string;
    ismap: boolean;
    width: number;
    height: number;
}

declare class Image extends HTMLImageElement {
    constructor(width?: number, height?: number): void;
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
    dynsrc: string;
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

declare class HTMLAnchorElement extends HTMLElement {
    accessKey: string;
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
    password: string;
    origin: string;
    pathname: string;
    port: string;
    protocol: string;
    rel: string;
    rev: string;
    search: string;
    shape: string;
    tabindex: number;
    text: string;
    type: string;
    username: string;

    // Note: these should be on base class HTMLElement.
    blur(): void;
    click(): void;
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

declare class HTMLAnchorElement extends HTMLElement {
    accessKey: string;
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
    password: string;
    origin: string;
    pathname: string;
    port: string;
    protocol: string;
    rel: string;
    rev: string;
    search: string;
    shape: string;
    tabindex: number;
    target: string;
    text: string;
    type: string;
    username: string;
    toString(): string;
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
    createHTMLDocument(title: string): Document;
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
