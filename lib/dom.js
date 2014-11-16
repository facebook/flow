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
    childNodes: NodeList;
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
    ENTITY_REFERENCE_NODE: number;
    ATTRIBUTE_NODE: number;
    DOCUMENT_FRAGMENT_NODE: number;
    TEXT_NODE: number;
    ELEMENT_NODE: number;
    COMMENT_NODE: number;
    DOCUMENT_POSITION_DISCONNECTED: number;
    DOCUMENT_POSITION_CONTAINED_BY: number;
    DOCUMENT_POSITION_CONTAINS: number;
    DOCUMENT_TYPE_NODE: number;
    DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC: number;
    DOCUMENT_NODE: number;
    ENTITY_NODE: number;
    PROCESSING_INSTRUCTION_NODE: number;
    CDATA_SECTION_NODE: number;
    NOTATION_NODE: number;
    DOCUMENT_POSITION_FOLLOWING: number;
    DOCUMENT_POSITION_PRECEDING: number;
}

declare class NodeList {
    length: number;
    item(index: number): Node;
    [index: number]: Node;
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
    getElementsByTagNameNS(namespaceURI: string, localName: string): NodeList;
    createElementNS(namespaceURI: string, qualifiedName: string): Element;
    open(url?: string, name?: string, features?: string, replace?: boolean): any;
    createAttributeNS(namespaceURI: string, qualifiedName: string): Attr;
    close(): void;
    getElementsByClassName(classNames: string): NodeList;
    importNode(importedNode: Node, deep: boolean): Node;
    createComment(data: string): Comment;
    getElementsByTagName(name: string): NodeList;
    createDocumentFragment(): Node;
    getElementsByName(elementName: string): NodeList;
    createAttribute(name: string): Attr;
    createTextNode(data: string): Text;
    xmlEncoding: string;
    xmlStandalone: boolean;
    xmlVersion: string;

    // extension
    location: Location;
    querySelectorAll(selectors: string): NodeList;
    querySelector(selectors: string): Element;
    createEvent(eventInterface: string): Event;
    createRange(): Range;
    elementFromPoint(x: number, y: number): Element;
    defaultView: any;
    compatMode: string;
    activeElement: Element;
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
    getElementsByTagNameNS(namespaceURI: string, localName: string): NodeList;
    hasAttributeNS(namespaceURI: string, localName: string): boolean;
    getAttributeNS(namespaceURI: string, localName: string): string;
    getAttributeNodeNS(namespaceURI: string, localName: string): Attr;
    setAttributeNodeNS(newAttr: Attr): Attr;
    hasAttribute(name: string): boolean;
    removeAttribute(name?: string): void;
    setAttributeNS(namespaceURI: string, qualifiedName: string, value: string): void;
    getAttributeNode(name: string): Attr;
    getElementsByTagName(name: string): NodeList;
    setAttributeNode(newAttr: Attr): Attr;
    removeAttributeNode(oldAttr: Attr): Attr;
    setAttribute(name?: string, value?: string): void;
    removeAttributeNS(namespaceURI: string, localName: string): void;
    querySelector(selector: string): Element;
    querySelectorAll(selector: string): NodeList;
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
