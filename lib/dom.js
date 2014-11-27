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
        type: string;
        endings: string;
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

/**
 * Definitions taken from https://dom.spec.whatwg.org/ and http://www.w3.org/TR/WebIDL
 */

type DOMString = string;
type DOMTimeStamp = number;

// Defined in http://www.w3.org/TR/dom/#domexception but will be moved to WebIDL
declare class DOMException {
    static INDEX_SIZE_ERR: number; // = 1;
    static DOMSTRING_SIZE_ERR: number; // = 2; // historical
    static HIERARCHY_REQUEST_ERR: number; // = 3;
    static WRONG_DOCUMENT_ERR: number; // = 4;
    static INVALID_CHARACTER_ERR: number; // = 5;
    static NO_DATA_ALLOWED_ERR: number; // = 6; // historical
    static NO_MODIFICATION_ALLOWED_ERR: number; // = 7;
    static NOT_FOUND_ERR: number; // = 8;
    static NOT_SUPPORTED_ERR: number; // = 9;
    static INUSE_ATTRIBUTE_ERR: number; // = 10; // historical
    static INVALID_STATE_ERR: number; // = 11;
    static SYNTAX_ERR: number; // = 12;
    static INVALID_MODIFICATION_ERR: number; // = 13;
    static NAMESPACE_ERR: number; // = 14;
    static INVALID_ACCESS_ERR: number; // = 15;
    static VALIDATION_ERR: number; // = 16; // historical
    static TYPE_MISMATCH_ERR: number; // = 17; // historical; use JavaScript's TypeError instead
    static SECURITY_ERR: number; // = 18;
    static NETWORK_ERR: number; // = 19;
    static ABORT_ERR: number; // = 20;
    static URL_MISMATCH_ERR: number; // = 21;
    static QUOTA_EXCEEDED_ERR: number; // = 22;
    static TIMEOUT_ERR: number; // = 23;
    static INVALID_NODE_TYPE_ERR: number; // = 24;
    static DATA_CLONE_ERR: number; // = 25;
    code: number;
}


// 3.2 Event

type EventInit = {
    bubbles: boolean /*=false*/;
    cancelable: boolean /*=false*/;
};

declare class Event {
    constructor(type: DOMString, eventInitDict?: EventInit): void;

    type: DOMString;
    target: ?EventTarget;
    currentTarget: ?EventTarget;

    // Phase constants
    static NONE: number; // = 0
    static CAPTURE_PHASE: number; // = 1
    static AT_TARGET: number; // = 2
    static BUBBLING_PHASE: number; // = 3
    currentPhase: number;

    stopPropagation(): void;
    stopImmediatePropagation(): void;

    bubbles: boolean;
    cancelable: boolean;
    preventDefault(): void;
    defaultPrevented: boolean;

    isTrusted: boolean;
    timeStamp: DOMTimeStamp;

    initEvent(type: DOMString, bubbles: boolean, cancelable: boolean): Event;

    // IE
    srcElement: Element;
    cancelBubble: boolean;
}

// 3.3 CustomEvent

type CustomEventInit = {
    detail: any /*=null*/;
    bubbles: boolean /*=false*/;
    cancelable: boolean /*=false*/;
};

declare class CustomEvent extends Event {
    constructor(type: DOMString, eventInitDict?: CustomEventInit): void;

    detail: any;
    initCustomEvent(type: DOMString, bubbles: boolean, cancelable: boolean): CustomEvent;
}

// 3.6 EventTarget

type callback = (evt: Event) => void;

declare class EventListener {
  handleEvent(event: Event): void;
}

declare class EventTarget {
    removeEventListener(
        type: string,
        callback: ?callback | ?EventListener,
        capture?: boolean
    ): void;
    addEventListener(
        type: string,
        callback: ?callback | ?EventListener,
        capture?: boolean
    ): void;
    dispatchEvent(evt: Event): boolean;
}


// 4.2.6 Collections: Elements

declare class Elements extends Array {
    query(relativeSelectors: DOMString): ?Element;
    queryAll(relativeSelectors: DOMString): Elements;
}

// 4.2.7 Old-style collections: NodeList and HTMLCollection

declare class NodeList<T> {
    [index: number]: ?T;
    item(index: number): ?T;
    length: number;
    // TODO: iterable<T>
}

declare class HTMLCollection {
    [index: number]: ?Element;
    // TODO: Multiple indexers
    // [name: string]: ?Element;
    item(index: number): ?Element;
    namedItem(name: string): ?Element;
    length: number;
}

// 4.3.1 Interface MutationObserver

declare class MutationObserver {
    constructor(callback: MutationCallback): void;
    observe(target: Node, options: MutationObserverInit): void;
    disconnect(): void;
    takeRecords(): Array<MutationRecord>;
}

type MutationCallback = (mutations: Array<MutationRecord>, observer: MutationObserver) => void;

type MutationObserverInit = {
    childList: boolean; // = false
    attributes: boolean;
    characterData: boolean;
    subtree: boolean; // = false
    attributeOldValue: boolean;
    characterDataOldValue: boolean;
    attributeFilter: Array<DOMString>;
};

// 4.3.3 Interface MutationRecord

declare class MutationRecord {
    type: DOMString;
    target: Node;
    addedNodes: NodeList<Node>;
    removedNodes: NodeList<Node>;
    previousSibling: ?Node;
    nextSibling: ?Node;
    attributeName: ?DOMString;
    attributeNamespace: ?DOMString;
    oldValue: ?DOMString;
}

// 4.4 Interface Node

declare class Node extends EventTarget {
    static ELEMENT_NODE: number; // = 1
    static ATTRIBUTE_NODE: number; // = 2 // historical
    static TEXT_NODE: number; // = 3
    static CDATA_SECTION_NODE: number; // = 4 // historical
    static ENTITY_REFERENCE_NODE: number; // = 5 // historical
    static ENTITY_NODE: number; // = 6 // historical
    static PROCESSING_INSTRUCTION_NODE: number; // = 7
    static COMMENT_NODE: number; // = 8
    static DOCUMENT_NODE: number; // = 9
    static DOCUMENT_TYPE_NODE: number; // = 10
    static DOCUMENT_FRAGMENT_NODE: number; // = 11
    static NOTATION_NODE: number; // = 12 // historical
    nodeType: number;
    nodeName: DOMString;

    baseURI: ?DOMString;

    ownerDocument: ?Document;
    parentNode: ?Node;
    parentElement: ?Element;
    hasChildNodes(): boolean;
    childNodes: NodeList<Node>;
    firstChild: ?Node;
    lastChild: ?Node;
    previousSibling: ?Node;
    nextSibling: ?Node;

    nodeValue: ?DOMString;
    textContent: ?DOMString;
    normalize(): void;

    cloneNode(deep?: boolean): Node;
    isEqualNode(node: ?Node): boolean;

    static DOCUMENT_POSITION_DISCONNECTED: number; // = 0x01;
    static DOCUMENT_POSITION_PRECEDING: number; // = 0x02;
    static DOCUMENT_POSITION_FOLLOWING: number; // = 0x04;
    static DOCUMENT_POSITION_CONTAINS: number; // = 0x08;
    static DOCUMENT_POSITION_CONTAINED_BY: number; // = 0x10;
    static DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC: number; // = 0x20;
    compareDocumentPosition(other: Node): number;
    contains(other: ?Node): boolean;

    lookupPrefix(namespace: ?DOMString): ?DOMString;
    lookupNamespaceURI(prefix: ?DOMString): ?DOMString;
    isDefaultNamespace(namespace: ?DOMString): boolean;

    insertBefore(node: Node, child: ?Node): Node;
    appendChild(node: Node): Node;
    replaceChild(node: Node, child: Node): Node;
    removeChild(node: Node): Node;
}

// 4.5 Interface Document

type Nodish = Node | DOMString;

declare class Document {
    implementation: DOMImplementation;
    URL: DOMString;
    documentURI: DOMString;
    origin: DOMString;
    compatMode: DOMString;
    characterSet: DOMString;
    inputEncoding: DOMString; // legacy alias of .characterSet
    contentType: DOMString;

    doctype: ?DocumentType;
    documentElement: ?Element;
    getElementsByTagName(localName: DOMString): HTMLCollection;
    getElementsByTagNameNS(namespace: ?DOMString, localName: DOMString): HTMLCollection;
    getElementsByClassName(classNames: DOMString): HTMLCollection;

    createElement(localName: DOMString): Element;
    createElementNS(namespace: ?DOMString, qualifiedName: DOMString): Element;
    createDocumentFragment(): DocumentFragment;
    createTextNode(data: DOMString): Text;
    createComment(data: DOMString): Comment;
    createProcessingInstruction(target: DOMString, data: DOMString): ProcessingInstruction;

    importNode(node: Node, deep?: boolean /* =false */): Node;
    adoptNode(node: Node): Node;

    createAttribute(localName: DOMString): Attr;
    createAttributeNS(namespace: ?DOMString, name: DOMString): Attr;

    createEvent(iface: DOMString): Event;

    createRange(): Range;

    // NODEFilter.SHOW_ALL = 0xFFFFFFF
    createNodeIterator(
        root: Node,
        whatToShow?: number, /* =0xFFFFFFF */
        filter?: ?NodeFilter
    ): NodeIterator;
    createTreeWalker(
        root: Node,
        whatToShow?: number, /* =0xFFFFFFF */
        filter?: ?NodeFilter
    ): TreeWalker;

    // from interface NonElementParentNode
    getElementById(elementId: DOMString): ?Element;

    // from interface ParentNode
    children: HTMLCollection;
    firstElementChild: ?Element;
    lastElementChild: ?Element;
    childElementCount: number;

    prepend(...nodes: Nodish[]): void;
    append(...nodes: Nodish[]): void;

    query(relativeSelectors: DOMString): ?Element;
    queryAll(relativeSelectors: DOMString): Elements;
    querySelector(selectors: DOMString): ?Element;
    querySelectorAll(selectors: DOMString): NodeList<Element>;
}

declare class XMLDocument extends Document {}

// 4.5.1 Interface DOMImplementation

declare class DOMImplementation {
  createDocumentType(
      qualifiedName: DOMString,
      publicId: DOMString,
      systemId: DOMString
  ): DocumentType;
  createDocument(
      namespace: ?DOMString,
      qualifiedName: ?DOMString,
      doctype?: ?DocumentType
  ): XMLDocument;
  createHTMLDocument(title?: DOMString): Document;

  hasFeature(): boolean; // useless; always returns true
}

// 4.6 Interface DocumentFragment

declare class DocumentFragment extends Node {
    // from interface NonElementParentNode
    getElementById(elementId: DOMString): ?Element;
    //
    // from interface ParentNode
    children: HTMLCollection;
    firstElementChild: ?Element;
    lastElementChild: ?Element;
    childElementCount: number;

    prepend(...nodes: Nodish[]): void;
    append(...nodes: Nodish[]): void;

    query(relativeSelectors: DOMString): ?Element;
    queryAll(relativeSelectors: DOMString): Elements;
    querySelector(selectors: DOMString): ?Element;
    querySelectorAll(selectors: DOMString): NodeList<Element>;
}

// 4.7 Interface DocumentType

declare class DocumentType extends Node {
    name: DOMString;
    publicId: DOMString;
    systemId: DOMString;

    // from interface ChildNode
    before(...nodes: Nodish[]): void;
    after(...nodes: Nodish[]): void;
    replace(...nodes: Nodish[]): void;
    remove(): void;
}

// 4.8 Interface Element

declare class Element extends Node {
    namespaceURI: ?DOMString;
    prefix: ?DOMString;
    localName: DOMString;
    tagName: DOMString;

    id: DOMString;
    className: DOMString;
    classList: DOMTokenList;

    hasAttributes(): boolean;
    attributes: NamedNodeMap;
    getAttribute(name: DOMString): ?DOMString;
    getAttributeNS(namespace: ?DOMString, localName: DOMString): ?DOMString;
    setAttribute(name: DOMString, value: DOMString): void;
    setAttributeNS(namespace: ?DOMString, name: DOMString, value: DOMString): void;
    removeAttribute(name: DOMString): void;
    removeAttributeNS(namespace: ?DOMString, localName: DOMString): void;
    hasAttribute(name: DOMString): boolean;
    hasAttributeNS(namespace: ?DOMString, localName: DOMString): boolean;

    getAttributeNode(name: DOMString): ?Attr;
    getAttributeNodeNS(namespace: ?DOMString, localName: DOMString): ?Attr;
    setAttributeNode(attr: Attr): ?Attr;
    setAttributeNodeNS(attr: Attr): ?Attr;
    removeAttributeNode(attr: Attr): Attr;

    closest(selectors: DOMString): ?Element;
    matches(selectors: DOMString): boolean;

    getElementsByTagName(localName: DOMString): HTMLCollection;
    getElementsByTagNameNS(namespace: ?DOMString, localName: DOMString): HTMLCollection;
    getElementsByClassName(classNames: DOMString): HTMLCollection;

    // from interface ParentNode
    children: HTMLCollection;
    firstElementChild: ?Element;
    lastElementChild: ?Element;
    childElementCount: number;

    prepend(...nodes: Nodish[]): void;
    append(...nodes: Nodish[]): void;

    query(relativeSelectors: DOMString): ?Element;
    queryAll(relativeSelectors: DOMString): Elements;
    querySelector(selectors: DOMString): ?Element;
    querySelectorAll(selectors: DOMString): NodeList<Element>;

    // from interface NonDocumentTypeChildNode
    previousElementSibling: ?Element;
    nextElementSibling: ?Element;


    // from interface ChildNode
    before(...nodes: Nodish[]): void;
    after(...nodes: Nodish[]): void;
    replace(...nodes: Nodish[]): void;
    remove(): void;
}

// 4.8.1 Interface NamedNodeMap

declare class NamedNodeMap {
    length: number;
    [index: number]: ?Attr;
    item(index: number): ?Attr;
    // TODO: Multiple indexers
    // [name: DOMString]: ?Attr;
    getNamedItem(name: DOMString): ?Attr;
    getNamedItemNS(namespace: ?DOMString, localName: DOMString): ?Attr;
    setNamedItem(attr: Attr): ?Attr;
    setNamedItemNS(attr: Attr): ?Attr;
    removeNamedItem(name: DOMString): Attr;
    removeNamedItemNS(namespace: ?DOMString, localName: DOMString): Attr;
}

// 4.8.2 Interface Attr

declare class Attr {
    namespaceURI: ?DOMString;
    prefix: ?DOMString;
    localName: DOMString;
    name: DOMString;
    value: DOMString;
    nodeValue: DOMString; // legacy alias of .value
    textContent: DOMString; // legacy alias of .value

    ownerElement: ?Element;
    specified: boolean; // useless; always returns true
}

// 4.9 Interface CharacterData

declare class CharacterData extends Node {
    data: DOMString;
    length: number;
    substringData(offset: number, count: number): DOMString;
    appendData(data: DOMString): void;
    insertData(offset: number, data: DOMString): void;
    deleteData(offset: number, count: number): void;
    replaceData(offset: number, count: number, data: DOMString): void;

    // from interface NonDocumentTypeChildNode
    previousElementSibling: ?Element;
    nextElementSibling: ?Element;

    // from interface ChildNode
    before(...nodes: Nodish[]): void;
    after(...nodes: Nodish[]): void;
    replace(...nodes: Nodish[]): void;
    remove(): void;
}

// 4.10 Interface Text

declare class Text extends CharacterData {
    constructor(data?: DOMString /*=""*/): void;
    splitText(offset: number): Text;
    wholeText: DOMString;
}

// 4.11 Interface ProcessingInstruction

declare class ProcessingInstruction extends CharacterData {
    target: DOMString;
}

// 4.12 Interface Comment

declare class Comment extends CharacterData {
    constructor(data?: DOMString /*=""*/): void;
}

// 5.2 Interface Range

declare class Range {
    startContainer: Node;
    startOffset: number;
    endContainer: Node;
    endOffset: number;
    collapsed: boolean;
    commonAncestorContainer: Node;

    setStart(node: Node, offset: number): void;
    setEnd(node: Node, offset: number): void;
    setStartBefore(node: Node): void;
    setStartAfter(node: Node): void;
    setEndBefore(node: Node): void;
    setEndAfter(node: Node): void;
    collapse(toStart?: boolean /*=false*/): void;
    selectNode(node: Node): void;
    selectNodeContents(node: Node): void;

    static START_TO_START: number; // = 0
    static START_TO_END: number; // = 1
    static END_TO_START: number; // = 2
    static END_TO_START: number; // = 3
    compareBoundaryPoints(how: number, sourceRange: Range): number;

    deleteContents(): void;
    extractContents(): DocumentFragment;
    cloneContents(): DocumentFragment;
    inserNode(node: Node): void;
    surroundContents(newParent: Node): void;

    cloneRange(): Range;
    detach(): void;
    isPointInRange(node: Node, offset: number): boolean;
    comparePoint(node: Node, offset: number): number;

    intersectsNode(node: Node): boolean;

    toString(): string;
}

// 6.1 Interface NodeIterator

declare class NodeIterator {
    root: Node;
    referenceNode: Node;
    pointerBeforeReferenceNode: boolean;
    whatToShow: number;
    filter: ?NodeFilter;

    nextNode(): ?Node;
    previousNode(): ?Node;

    detach(): void;
}

// 6.2 Interface TreeWalker

declare class TreeWalker {
    root: Node;
    whatToShow: number;
    filter: ?NodeFilter;
    currentNode: Node;

    parenNode(): ?Node;
    firstChild(): ?Node;
    lastChild(): ?Node;
    previousSibling(): ?Node;
    nextSibling(): ?Node;
    previousNode(): ?Node;
    nextNode(): ?Node;
}

// 6.3 Interface NodeFilter

declare class NodeFilter {
    // Constants for acceptNode()
    static FILTER_ACCEPT: number; // = 1;
    static FILTER_REJECT: number; // = 2;
    static FILTER_SKIP: number; // = 3;

    // Constants for whatToShow
    static SHOW_ALL: number; // = 0xFFFFFFFF;
    static SHOW_ELEMENT: number; // = 0x1;
    static SHOW_ATTRIBUTE: number; // = 0x2; // historical
    static SHOW_TEXT: number; // = 0x4;
    static SHOW_CDATA_SECTION: number; // = 0x8; // historical
    static SHOW_ENTITY_REFERENCE: number; // = 0x10; // historical
    static SHOW_ENTITY: number; // = 0x20; // historical
    static SHOW_PROCESSING_INSTRUCTION: number; // = 0x40;
    static SHOW_COMMENT: number; // = 0x80;
    static SHOW_DOCUMENT: number; // = 0x100;
    static SHOW_DOCUMENT_TYPE: number; // = 0x200;
    static SHOW_DOCUMENT_FRAGMENT: number; // = 0x400;
    static SHOW_NOTATION: number; // = 0x800; // historical

    acceptNode(node: Node): number;
}

// 7.1 Interface DOMTokenList

declare class DOMTokenList {
    length: number;
    [index: number]: ?DOMString;
    item(index: number): ?DOMString;
    contains(token: DOMString): boolean;
    add(...tokens: DOMString[]): void;
    remove(...tokens: DOMString[]): void;
    toggle(token: DOMString, force?: boolean): boolean;
    toString(): string;
    // TODO: iterable<DOMString>;
}

// 7.2 Interface DOMSettableTokenList

declare class DOMSettableTokenList extends DOMTokenList {
    value: DOMString;
}

/* Browser globals */

declare var document:  Document;

/* HTML */

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
