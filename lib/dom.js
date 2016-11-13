/**
* Copyright (c) 2013-present, Facebook, Inc.
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
  isClosed: bool;
  size: number;
  type: string;
  close(): void;
  slice(start?: number, end?: number, contentType?: string): Blob;
}

declare class FileReader extends EventTarget {
  abort(): void;
  DONE: number;
  EMPTY: number;
  error: DOMError;
  LOADING: number;
  onabort: (ev: any) => any;
  onerror: (ev: any) => any;
  onload: (ev: any) => any;
  onloadend: (ev: any) => any;
  onloadstart: (ev: any) => any;
  onprogress: (ev: any) => any;
  readAsArrayBuffer(blob: Blob): void;
  readAsDataURL(blob: Blob): void;
  readAsText(blob: Blob, encoding?: string): void;
  readyState: 0 | 1 | 2;
  result: any;
}

declare class File extends Blob {
  lastModifiedDate: any;
  name: string;
}

declare class FileList {
  @@iterator(): Iterator<File>;
  length: number;
  item(index: number): File;
  [index: number]: File;
}

/* DataTransfer */

declare class DataTransfer {
  clearData(format?: string): void;
  getData(format: string): string;
  setData(format: string, data: string): void;
  setDragImage(image: Element, x: number, y: number): void;
  dropEffect: string;
  effectAllowed: string;
  files: FileList; // readonly
  items: DataTransferItemList; // readonly
  types: Array<string>; // readonly
}

declare class DataTransferItemList {
  length: number; // readonly
  [index: number]: DataTransferItem;
  add(data: string, type: string): ?DataTransferItem;
  add(data: File): ?DataTransferItem;
  remove(index: number): void;
  clear(): void;
};

declare class DataTransferItem {
  kind: string; // readonly
  type: string; // readonly
  getAsString(_callback: ?(data: string) => mixed): void;
  getAsFile(): ?File;
};

/* DOM */

declare class DOMError {
  name: string;
}

type EventHandler = (event: Event) => mixed
type EventListener = {handleEvent: EventHandler} | EventHandler
type MouseEventHandler = (event: MouseEvent) => mixed
type MouseEventListener = {handleEvent: MouseEventHandler} | MouseEventHandler
type KeyboardEventHandler = (event: KeyboardEvent) => mixed
type KeyboardEventListener = {handleEvent: KeyboardEventHandler} | KeyboardEventHandler
type TouchEventHandler = (event: TouchEvent) => mixed
type TouchEventListener = {handleEvent: TouchEventHandler} | TouchEventHandler
type WheelEventHandler = (event: WheelEvent) => mixed
type WheelEventListener = {handleEvent: WheelEventHandler} | WheelEventHandler

type MouseEventTypes = 'contextmenu' | 'mousedown' | 'mouseenter' | 'mouseleave' | 'mousemove' | 'mouseout' | 'mouseover' | 'mouseup' | 'click' | 'dblclick';
type KeyboardEventTypes = 'keydown' | 'keyup' | 'keypress';
type TouchEventTypes = 'touchstart' | 'touchmove' | 'touchend' | 'touchcancel';
type WheelEventTypes = 'wheel';

declare class EventTarget {
  addEventListener(type: MouseEventTypes, listener: MouseEventListener, useCapture?: boolean): void;
  addEventListener(type: KeyboardEventTypes, listener: KeyboardEventListener, useCapture?: boolean): void;
  addEventListener(type: TouchEventTypes, listener: TouchEventListener, useCapture?: boolean): void;
  addEventListener(type: WheelEventTypes, listener: WheelEventListener, useCapture?: boolean): void;
  addEventListener(type: string, listener: EventListener, useCapture?: boolean): void;

  removeEventListener(type: MouseEventTypes, listener: MouseEventListener, useCapture?: boolean): void;
  removeEventListener(type: KeyboardEventTypes, listener: KeyboardEventListener, useCapture?: boolean): void;
  removeEventListener(type: TouchEventTypes, listener: TouchEventListener, useCapture?: boolean): void;
  removeEventListener(type: WheelEventTypes, listener: WheelEventListener, useCapture?: boolean): void;
  removeEventListener(type: string, listener: EventListener, useCapture?: boolean): void;

  attachEvent?: (type: MouseEventTypes, listener: MouseEventListener) => void;
  attachEvent?: (type: KeyboardEventTypes, listener: KeyboardEventListener) => void;
  attachEvent?: (type: TouchEventTypes, listener: TouchEventListener) => void;
  attachEvent?: (type: WheelEventTypes, listener: WheelEventListener) => void;
  attachEvent?: (type: string, listener: EventListener) => void;

  detachEvent?: (type: MouseEventTypes, listener: MouseEventListener) => void;
  detachEvent?: (type: KeyboardEventTypes, listener: KeyboardEventListener) => void;
  detachEvent?: (type: TouchEventTypes, listener: TouchEventListener) => void;
  detachEvent?: (type: WheelEventTypes, listener: WheelEventListener) => void;
  detachEvent?: (type: string, listener: EventListener) => void;

  dispatchEvent(evt: Event): boolean;

  // Deprecated

  cancelBubble: boolean;
  initEvent(eventTypeArg: string, canBubbleArg: boolean, cancelableArg: boolean): void;
}

type Event$Init = {
  bubbles?: boolean,
  cancelable?: boolean,
  composed?: boolean,
}

declare class Event {
  constructor(type: string, eventInitDict?: Event$Init): void;
  bubbles: boolean;
  cancelable: boolean;
  currentTarget: EventTarget;
  defaultPrevented: boolean;
  eventPhase: number;
  isTrusted: boolean;
  srcElement: Element;
  target: EventTarget;
  timeStamp: number;
  type: string;
  preventDefault(): void;
  stopImmediatePropagation(): void;
  stopPropagation(): void;
  AT_TARGET: number;
  BUBBLING_PHASE: number;
  CAPTURING_PHASE: number;

  // deprecated
  initEvent(
    type: string,
    bubbles: boolean,
    cancelable: boolean
  ): void;
}

type CustomEvent$Init = Event$Init & {
  detail?: any;
}

declare class CustomEvent extends Event {
  constructor(type: string, eventInitDict?: CustomEvent$Init): void;
  detail: any;

  // deprecated
  initCustomEvent(
    type: string,
    bubbles: boolean,
    cancelable: boolean,
    detail: any
  ): CustomEvent;
}

declare class UIEvent extends Event {
  detail: number;
  view: any;
}

declare class MouseEvent extends UIEvent {
  altKey: boolean;
  button: number;
  buttons: number;
  clientX: number;
  clientY: number;
  ctrlKey: boolean;
  metaKey: boolean;
  movementX: number;
  movementY: number;
  offsetX: number;
  offsetY: number;
  pageX: number;
  pageY: number;
  region: ?string;
  screenX: number;
  screenY: number;
  shiftKey: boolean;
  relatedTarget: ?EventTarget;
  getModifierState(keyArg: string): boolean;
}

declare class WheelEvent extends MouseEvent {
  deltaX: number; // readonly
  deltaY: number; // readonly
  deltaZ: number; // readonly
  deltaMode: 0x00 | 0x01 | 0x02; // readonly
}

declare class DragEvent extends MouseEvent {
  dataTransfer: ?DataTransfer; // readonly
}

declare class ProgressEvent extends Event {
  lengthComputable: boolean;
  loaded: number;
  total: number;

  // Deprecated
  initProgressEvent(
    typeArg: string,
    canBubbleArg: boolean,
    cancelableArg: boolean,
    lengthComputableArg: boolean,
    loadedArg: number,
    totalArg: number
  ): void;
}

declare class PromiseRejectionEvent extends Event {
  promise: Promise<any>;
  reason: any;
}

// used for websockets, for example. See:
// http://www.w3.org/TR/2011/WD-websockets-20110419/
// and
// http://www.w3.org/TR/2008/WD-html5-20080610/comms.html
declare class MessageEvent extends Event {
  data: string | Blob | ArrayBuffer;
  origin: string;
  lastEventId: string;
  source: WindowProxy;
}

declare class KeyboardEvent extends UIEvent {
  altKey: boolean;
  code: string;
  ctrlKey: boolean;
  isComposing: boolean;
  key: string;
  location: number;
  metaKey: boolean;
  repeat: boolean;
  shiftKey: boolean;
  getModifierState(keyArg?: string): boolean;

  // Deprecated
  charCode: number;
  keyCode: number;
  which: number;
}

// https://www.w3.org/TR/touch-events/#idl-def-Touch
declare class Touch {
  clientX: number,
  clientY: number,
  identifier: number,
  pageX: number,
  pageY: number,
  screenX: number,
  screenY: number,
  target: EventTarget,
}

// https://www.w3.org/TR/touch-events/#idl-def-TouchList
// TouchList#item(index) will return null if n > #length. Should #item's
// return type just been Touch?
declare class TouchList {
  length: number,
  item(index: number): null | Touch,
  [index: number]: Touch,
}

// https://www.w3.org/TR/touch-events/#touchevent-interface
declare class TouchEvent extends UIEvent {
  altKey: boolean,
  changedTouches: TouchList,
  ctrlKey: boolean,
  metaKey: boolean,
  shiftKey: boolean,
  targetTouchesRead: TouchList,
  touches: TouchList,
}

// TODO: *Event

declare class Node extends EventTarget {
  baseURI: ?string;
  childNodes: NodeList<Node>;
  firstChild: ?Node;
  lastChild: ?Node;
  nextSibling: ?Node;
  nodeName: string;
  nodeType: number;
  nodeValue: string;
  ownerDocument: Document;
  parentElement: ?Element;
  parentNode: ?Node;
  previousSibling: ?Node;
  rootNode: Node;
  textContent: string;
  appendChild<T: Node>(newChild: T): T;
  cloneNode(deep?: boolean): this;
  compareDocumentPosition(other: Node): number;
  contains(other: ?Node): boolean;
  hasChildNodes(): boolean;
  insertBefore<T: Node>(newChild: T, refChild?: ?Node): T;
  isDefaultNamespace(namespaceURI: string): boolean;
  isEqualNode(arg: Node): boolean;
  isSameNode(other: Node): boolean;
  lookupNamespaceURI(prefix: string): string;
  lookupPrefix(namespaceURI: string): string;
  normalize(): void;
  removeChild<T: Node>(oldChild: T): T;
  replaceChild<T: Node>(newChild: Node, oldChild: T): T;
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

  // Non-standard
  innerText?: string;
  outerText?: string;
}

declare class NodeList<T> {
  @@iterator(): Iterator<T>;
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

declare class HTMLCollection<Elem: HTMLElement> {
  @@iterator(): Iterator<Elem>;
  length: number;
  item(nameOrIndex?: any, optionalIndex?: any): Elem;
  namedItem(name: string): Elem;
  [index: number]: Elem;
}

// from https://www.w3.org/TR/custom-elements/#extensions-to-document-interface-to-register
type ElementRegistrationOptions = {
  +prototype?: {
    // from https://www.w3.org/TR/custom-elements/#types-of-callbacks
    +createdCallback?: () => mixed;
    +attachedCallback?: () => mixed;
    +detachedCallback?: () => mixed;
    +attributeChangedCallback?:
    // attribute is set
    ((
      attributeLocalName: string,
      oldAttributeValue: null,
      newAttributeValue: string,
      attributeNamespace: string
    ) => mixed) &
    // attribute is changed
    ((
      attributeLocalName: string,
      oldAttributeValue: string,
      newAttributeValue: string,
      attributeNamespace: string
    ) => mixed) &
    // attribute is removed
    ((
      attributeLocalName: string,
      oldAttributeValue: string,
      newAttributeValue: null,
      attributeNamespace: string
    ) => mixed);
  };
  +extends?: string;
}

declare class Document extends Node {
  URL: string;
  adoptNode<T: Node>(source: T): T;
  anchors: HTMLCollection<HTMLAnchorElement>;
  applets: HTMLCollection<HTMLAppletElement>;
  body: HTMLElement;
  characterSet: string;
  close(): void;
  cookie: string;
  createAttribute(name: string): Attr;
  createAttributeNS(namespaceURI: string, qualifiedName: string): Attr;
  createCDATASection(data: string): Text;
  createComment(data: string): Comment;
  createDocumentFragment(): DocumentFragment;
  createElement(tagName: 'a'): HTMLAnchorElement;
  createElement(tagName: 'audio'): HTMLAudioElement;
  createElement(tagName: 'button'): HTMLButtonElement;
  createElement(tagName: 'canvas'): HTMLCanvasElement;
  createElement(tagName: 'div'): HTMLDivElement;
  createElement(tagName: 'form'): HTMLFormElement;
  createElement(tagName: 'iframe'): HTMLIFrameElement;
  createElement(tagName: 'img'): HTMLImageElement;
  createElement(tagName: 'input'): HTMLInputElement;
  createElement(tagName: 'label'): HTMLLabelElement;
  createElement(tagName: 'link'): HTMLLinkElement;
  createElement(tagName: 'media'): HTMLMediaElement;
  createElement(tagName: 'meta'): HTMLMetaElement;
  createElement(tagName: 'option'): HTMLOptionElement;
  createElement(tagName: 'p'): HTMLParagraphElement;
  createElement(tagName: 'script'): HTMLScriptElement;
  createElement(tagName: 'select'): HTMLSelectElement;
  createElement(tagName: 'source'): HTMLSourceElement;
  createElement(tagName: 'span'): HTMLSpanElement;
  createElement(tagName: 'style'): HTMLStyleElement;
  createElement(tagName: 'textarea'): HTMLTextAreaElement;
  createElement(tagName: 'video'): HTMLVideoElement;
  createElement(tagName: 'tr'): HTMLTableRowElement;
  createElement(tagName: 'td' | 'th'): HTMLTableCellElement;
  createElement(tagName: string): HTMLElement;
  createElementNS(namespaceURI: string, qualifiedName: string): Element;
  createTextNode(data: string): Text;
  currentScript: HTMLScriptElement | null;
  doctype: DocumentType;
  documentElement: HTMLElement;
  documentMode: number;
  domain: string | null;
  embeds: HTMLCollection<HTMLEmbedElement>;
  execCommand(cmdID: string, showUI?: boolean, value?: any): boolean;
  forms: HTMLCollection<HTMLFormElement>;
  getElementById(elementId: string): HTMLElement;
  getElementsByClassName(classNames: string): HTMLCollection<HTMLElement>;
  getElementsByName(elementName: string): HTMLCollection<HTMLElement>;
  getElementsByTagName(name: 'a'): HTMLCollection<HTMLAnchorElement>;
  getElementsByTagName(name: 'audio'): HTMLCollection<HTMLAudioElement>;
  getElementsByTagName(name: 'button'): HTMLCollection<HTMLButtonElement>;
  getElementsByTagName(name: 'canvas'): HTMLCollection<HTMLCanvasElement>;
  getElementsByTagName(name: 'div'): HTMLCollection<HTMLDivElement>;
  getElementsByTagName(name: 'form'): HTMLCollection<HTMLFormElement>;
  getElementsByTagName(name: 'iframe'): HTMLCollection<HTMLIFrameElement>;
  getElementsByTagName(name: 'img'): HTMLCollection<HTMLImageElement>;
  getElementsByTagName(name: 'input'): HTMLCollection<HTMLInputElement>;
  getElementsByTagName(name: 'label'): HTMLCollection<HTMLLabelElement>;
  getElementsByTagName(name: 'link'): HTMLCollection<HTMLLinkElement>;
  getElementsByTagName(name: 'media'): HTMLCollection<HTMLMediaElement>;
  getElementsByTagName(name: 'meta'): HTMLCollection<HTMLMetaElement>;
  getElementsByTagName(name: 'option'): HTMLCollection<HTMLOptionElement>;
  getElementsByTagName(name: 'p'): HTMLCollection<HTMLParagraphElement>;
  getElementsByTagName(name: 'script'): HTMLCollection<HTMLScriptElement>;
  getElementsByTagName(name: 'select'): HTMLCollection<HTMLSelectElement>;
  getElementsByTagName(name: 'source'): HTMLCollection<HTMLSourceElement>;
  getElementsByTagName(name: 'span'): HTMLCollection<HTMLSpanElement>;
  getElementsByTagName(name: 'style'): HTMLCollection<HTMLStyleElement>;
  getElementsByTagName(name: 'textarea'): HTMLCollection<HTMLTextAreaElement>;
  getElementsByTagName(name: 'video'): HTMLCollection<HTMLVideoElement>;
  getElementsByTagName(name: 'tr'): HTMLCollection<HTMLTableRowElement>;
  getElementsByTagName(name: 'td' | 'th'): HTMLCollection<HTMLTableCellElement>;
  getElementsByTagName(name: string): HTMLCollection<HTMLElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'a'): HTMLCollection<HTMLAnchorElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'audio'): HTMLCollection<HTMLAudioElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'button'): HTMLCollection<HTMLButtonElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'canvas'): HTMLCollection<HTMLCanvasElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'div'): HTMLCollection<HTMLDivElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'form'): HTMLCollection<HTMLFormElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'iframe'): HTMLCollection<HTMLIFrameElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'img'): HTMLCollection<HTMLImageElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'input'): HTMLCollection<HTMLInputElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'label'): HTMLCollection<HTMLLabelElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'link'): HTMLCollection<HTMLLinkElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'media'): HTMLCollection<HTMLMediaElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'meta'): HTMLCollection<HTMLMetaElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'option'): HTMLCollection<HTMLOptionElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'p'): HTMLCollection<HTMLParagraphElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'script'): HTMLCollection<HTMLScriptElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'select'): HTMLCollection<HTMLSelectElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'source'): HTMLCollection<HTMLSourceElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'span'): HTMLCollection<HTMLSpanElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'style'): HTMLCollection<HTMLStyleElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'textarea'): HTMLCollection<HTMLTextAreaElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'video'): HTMLCollection<HTMLVideoElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'tr'): HTMLCollection<HTMLTableRowElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'td' | 'th'): HTMLCollection<HTMLTableCellElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: string): HTMLCollection<HTMLElement>;
  head: HTMLElement;
  images: HTMLCollection<HTMLImageElement>;
  implementation: DOMImplementation;
  importNode<T: Node>(importedNode: T, deep: boolean): T;
  inputEncoding: string;
  lastModified: string;
  links: HTMLCollection<HTMLLinkElement>;
  media: string;
  open(url?: string, name?: string, features?: string, replace?: boolean): any;
  readyState: string;
  referrer: string;
  scripts: HTMLCollection<HTMLScriptElement>;
  styleSheets: StyleSheetList;
  title: string;
  visibilityState: 'visible' | 'hidden' | 'prerender' | 'unloaded';
  write(...content: Array<string>): void;
  writeln(...content: Array<string>): void;
  xmlEncoding: string;
  xmlStandalone: boolean;
  xmlVersion: string;

  registerElement(type: string, options?: ElementRegistrationOptions): any;
  getSelection(): Selection | null;

  // 6.4.6 Focus management APIs
  activeElement: HTMLElement;
  hasFocus(): boolean;

  // extension
  location: Location;
  createEvent(eventInterface: 'CustomEvent'): CustomEvent;
  createEvent(eventInterface: string): Event;
  createRange(): Range;
  elementFromPoint(x: number, y: number): HTMLElement;
  defaultView: any;
  compatMode: 'BackCompat' | 'CSS1Compat';
  hidden: boolean;

  // from ParentNode interface
  childElementCount: number;
  children: HTMLCollection<HTMLElement>;
  firstElementChild: ?Element;
  lastElementChild: ?Element;
  querySelector(selector: string): HTMLElement;
  querySelectorAll(selector: string): NodeList<HTMLElement>;

  // Interface DocumentTraversal
  // http://www.w3.org/TR/2000/REC-DOM-Level-2-Traversal-Range-20001113/traversal.html#Traversal-Document

  // Not all combinations of RootNodeT and whatToShow are logically possible.
  // The bitmasks NodeFilter.SHOW_CDATA_SECTION,
  // NodeFilter.SHOW_ENTITY_REFERENCE, NodeFilter.SHOW_ENTITY, and
  // NodeFilter.SHOW_NOTATION are deprecated and do not correspond to types
  // that Flow knows about.

  // NodeFilter.SHOW_ATTRIBUTE is also deprecated, but corresponds to the
  // type Attr. While there is no reason to prefer it to Node.attributes,
  // it does have meaning and can be typed: When (whatToShow &
  // NodeFilter.SHOW_ATTRIBUTE === 1), RootNodeT must be Attr, and when
  // RootNodeT is Attr, bitmasks other than NodeFilter.SHOW_ATTRIBUTE are
  // meaningless.
  createNodeIterator<RootNodeT: Attr>(root: RootNodeT, whatToShow: 2, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Attr>;
  createTreeWalker<RootNodeT: Attr>(root: RootNodeT, whatToShow: 2, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Attr>;

  // NodeFilter.SHOW_PROCESSING_INSTRUCTION is not implemented because Flow
  // does not currently define a ProcessingInstruction class.

  // When (whatToShow & NodeFilter.SHOW_DOCUMENT === 1 || whatToShow &
  // NodeFilter.SHOW_DOCUMENT_TYPE === 1), RootNodeT must be Document.
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 256, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Document>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 257, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Document|Element>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 260, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Document|Text>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 261, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Document|Element|Text>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 384, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Document|Comment>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 385, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Document|Element|Comment>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 388, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Document|Text|Comment>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 389, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Document|Element|Text|Comment>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 512, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 513, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Element>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 516, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Text>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 517, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Element|Text>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 640, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Comment>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 641, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Element|Comment>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 644, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Text|Comment>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 645, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Element|Text|Comment>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 768, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Document>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 769, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Document|Element>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 772, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Document|Text>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 773, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Document|Element|Text>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 896, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Document|Comment>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 897, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Document|Element|Comment>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 900, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Document|Text|Comment>;
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: 901, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentType|Document|Element|Text|Comment>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 256, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Document>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 257, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Document|Element>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 260, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Document|Text>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 261, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Document|Element|Text>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 384, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Document|Comment>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 385, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Document|Element|Comment>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 388, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Document|Text|Comment>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 389, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Document|Element|Text|Comment>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 512, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 513, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Element>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 516, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Text>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 517, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Element|Text>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 640, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Comment>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 641, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Element|Comment>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 644, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Text|Comment>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 645, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Element|Text|Comment>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 768, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Document>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 769, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Document|Element>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 772, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Document|Text>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 773, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Document|Element|Text>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 896, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Document|Comment>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 897, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Document|Element|Comment>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 900, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Document|Text|Comment>;
  createTreeWalker<RootNodeT: Document>(root: RootNodeT, whatToShow: 901, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentType|Document|Element|Text|Comment>;

  // When (whatToShow & NodeFilter.SHOW_DOCUMENT_FRAGMENT === 1), RootNodeT
  // must be a DocumentFragment.
  createNodeIterator<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1024, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentFragment>;
  createNodeIterator<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1025, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentFragment|Element>;
  createNodeIterator<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1028, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentFragment|Text>;
  createNodeIterator<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1029, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentFragment|Element|Text>;
  createNodeIterator<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1152, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentFragment|Comment>;
  createNodeIterator<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1153, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentFragment|Element|Comment>;
  createNodeIterator<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1156, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentFragment|Text|Comment>;
  createNodeIterator<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1157, filter?: NodeFilterInterface): NodeIterator<RootNodeT, DocumentFragment|Element|Text|Comment>;
  createTreeWalker<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1024, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentFragment>;
  createTreeWalker<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1025, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentFragment|Element>;
  createTreeWalker<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1028, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentFragment|Text>;
  createTreeWalker<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1029, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentFragment|Element|Text>;
  createTreeWalker<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1152, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentFragment|Comment>;
  createTreeWalker<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1153, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentFragment|Element|Comment>;
  createTreeWalker<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1156, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentFragment|Text|Comment>;
  createTreeWalker<RootNodeT: DocumentFragment>(root: RootNodeT, whatToShow: 1157, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, DocumentFragment|Element|Text|Comment>;

  // In the general case, RootNodeT may be any Node and whatToShow may be
  // NodeFilter.SHOW_ALL or any combination of NodeFilter.SHOW_ELEMENT,
  // NodeFilter.SHOW_TEXT and/or NodeFilter.SHOW_COMMENT
  createNodeIterator<RootNodeT: Node>(root: RootNodeT, whatToShow: 1, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Element>;
  createNodeIterator<RootNodeT: Node>(root: RootNodeT, whatToShow: 4, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Text>;
  createNodeIterator<RootNodeT: Node>(root: RootNodeT, whatToShow: 5, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Element|Text>;
  createNodeIterator<RootNodeT: Node>(root: RootNodeT, whatToShow: 128, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Comment>;
  createNodeIterator<RootNodeT: Node>(root: RootNodeT, whatToShow: 129, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Element|Comment>;
  createNodeIterator<RootNodeT: Node>(root: RootNodeT, whatToShow: 132, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Text|Comment>;
  createNodeIterator<RootNodeT: Node>(root: RootNodeT, whatToShow: 133, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Text|Element|Comment>;
  createNodeIterator<RootNodeT: Node>(root: RootNodeT, whatToShow: -1, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Node>;
  createTreeWalker<RootNodeT: Node>(root: RootNodeT, whatToShow: 1, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Element>;
  createTreeWalker<RootNodeT: Node>(root: RootNodeT, whatToShow: 4, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Text>;
  createTreeWalker<RootNodeT: Node>(root: RootNodeT, whatToShow: 5, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Element|Text>;
  createTreeWalker<RootNodeT: Node>(root: RootNodeT, whatToShow: 128, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Comment>;
  createTreeWalker<RootNodeT: Node>(root: RootNodeT, whatToShow: 129, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Element|Comment>;
  createTreeWalker<RootNodeT: Node>(root: RootNodeT, whatToShow: 132, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Text|Comment>;
  createTreeWalker<RootNodeT: Node>(root: RootNodeT, whatToShow: 133, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Text|Element|Comment>;
  createTreeWalker<RootNodeT: Node>(root: RootNodeT, whatToShow: -1, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Node>;

  // Catch all for when we don't know the value of `whatToShow`
  createNodeIterator<RootNodeT: Document>(root: RootNodeT, whatToShow: number, filter?: NodeFilterInterface): NodeIterator<RootNodeT, Node>;
  createTreeWalker<RootNodeT: Node>(root: RootNodeT, whatToShow: number, filter?: NodeFilterInterface, entityReferenceExpansion?: boolean): TreeWalker<RootNodeT, Node>;

  // And for when whatToShow is not provided, it is assumed to be SHOW_ALL
  createNodeIterator<RootNodeT: Node>(root: RootNodeT, whatToShow: void): NodeIterator<RootNodeT, Node>;
  createTreeWalker<RootNodeT: Node>(root: RootNodeT, whatToShow: void): TreeWalker<RootNodeT, Node>;
}

declare class DocumentFragment extends Node {
  // from ParentNode interface
  childElementCount: number;
  children: HTMLCollection<HTMLElement>;
  firstElementChild: ?Element;
  lastElementChild: ?Element;
  querySelector(selector: string): HTMLElement;
  querySelectorAll(selector: string): NodeList<HTMLElement>;
}

declare class Selection {
  anchorNode: Node | null;
  anchorOffset: number;
  focusNode: Node | null;
  focusOffset: number;
  isCollapsed: boolean;
  rangeCount: number;
  type: string;
  addRange(range: Range): void;
  getRangeAt(index: number): Range;
  removeRange(range: Range): void;
  removeAllRanges(): void;
  collapse(parentNode: Node | null, offset?: number): void;
  collapseToStart(): void;
  collapseToEnd(): void;
  containsNode(aNode: Node, aPartlyContained?: boolean): boolean;
  deleteFromDocument(): void;
  extend(parentNode: Node, offset?: number): void;
  empty(): void;
  selectAllChildren(parentNode: Node): void;
  setPosition(aNode: Node | null, offset?: number): void;
  setBaseAndExtent(anchorNode: Node, anchorOffset: number, focusNode: Node, focusOffset: number): void;
  toString(): string;
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
  extractContents(): DocumentFragment;
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
  attributes: NamedNodeMap;
  childElementCount: number;
  children: HTMLCollection<HTMLElement>;
  classList: DOMTokenList;
  className: string;
  clientHeight: number;
  clientLeft: number;
  clientTop: number;
  clientWidth: number;
  firstElementChild: ?Element;
  id: string;
  innerHTML: string;
  lastElementChild: ?Element;
  localName: string;
  namespaceURI: ?string;
  nextElementSibling: ?Element;
  outerHTML: string;
  prefix: string;
  previousElementSibling: ?Element;
  scrollHeight: number;
  scrollLeft: number;
  scrollTop: number;
  scrollWidth: number;
  tagName: string;

  closest(selectors: string): ?Element;
  dispatchEvent(event: Event): bool;
  getAttribute(name?: string): string;
  getAttributeNS(namespaceURI: string, localName: string): string;
  getAttributeNode(name: string): Attr;
  getAttributeNodeNS(namespaceURI: string, localName: string): Attr;
  getBoundingClientRect(): ClientRect;
  getClientRects(): ClientRect[];
  getElementsByClassName(names: string): HTMLCollection<HTMLElement>;
  getElementsByTagName(name: 'a'): HTMLCollection<HTMLAnchorElement>;
  getElementsByTagName(name: 'audio'): HTMLCollection<HTMLAudioElement>;
  getElementsByTagName(name: 'button'): HTMLCollection<HTMLButtonElement>;
  getElementsByTagName(name: 'canvas'): HTMLCollection<HTMLCanvasElement>;
  getElementsByTagName(name: 'div'): HTMLCollection<HTMLDivElement>;
  getElementsByTagName(name: 'form'): HTMLCollection<HTMLFormElement>;
  getElementsByTagName(name: 'iframe'): HTMLCollection<HTMLIFrameElement>;
  getElementsByTagName(name: 'img'): HTMLCollection<HTMLImageElement>;
  getElementsByTagName(name: 'input'): HTMLCollection<HTMLInputElement>;
  getElementsByTagName(name: 'label'): HTMLCollection<HTMLLabelElement>;
  getElementsByTagName(name: 'link'): HTMLCollection<HTMLLinkElement>;
  getElementsByTagName(name: 'media'): HTMLCollection<HTMLMediaElement>;
  getElementsByTagName(name: 'meta'): HTMLCollection<HTMLMetaElement>;
  getElementsByTagName(name: 'option'): HTMLCollection<HTMLOptionElement>;
  getElementsByTagName(name: 'p'): HTMLCollection<HTMLParagraphElement>;
  getElementsByTagName(name: 'script'): HTMLCollection<HTMLScriptElement>;
  getElementsByTagName(name: 'select'): HTMLCollection<HTMLSelectElement>;
  getElementsByTagName(name: 'source'): HTMLCollection<HTMLSourceElement>;
  getElementsByTagName(name: 'span'): HTMLCollection<HTMLSpanElement>;
  getElementsByTagName(name: 'style'): HTMLCollection<HTMLStyleElement>;
  getElementsByTagName(name: 'textarea'): HTMLCollection<HTMLTextAreaElement>;
  getElementsByTagName(name: 'video'): HTMLCollection<HTMLVideoElement>;
  getElementsByTagName(name: 'tr'): HTMLCollection<HTMLTableRowElement>;
  getElementsByTagName(name: 'td' | 'th'): HTMLCollection<HTMLTableCellElement>;
  getElementsByTagName(name: string): HTMLCollection<HTMLElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'a'): HTMLCollection<HTMLAnchorElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'audio'): HTMLCollection<HTMLAudioElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'button'): HTMLCollection<HTMLButtonElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'canvas'): HTMLCollection<HTMLCanvasElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'div'): HTMLCollection<HTMLDivElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'form'): HTMLCollection<HTMLFormElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'iframe'): HTMLCollection<HTMLIFrameElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'img'): HTMLCollection<HTMLImageElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'input'): HTMLCollection<HTMLInputElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'label'): HTMLCollection<HTMLLabelElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'link'): HTMLCollection<HTMLLinkElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'media'): HTMLCollection<HTMLMediaElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'meta'): HTMLCollection<HTMLMetaElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'option'): HTMLCollection<HTMLOptionElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'p'): HTMLCollection<HTMLParagraphElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'script'): HTMLCollection<HTMLScriptElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'select'): HTMLCollection<HTMLSelectElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'source'): HTMLCollection<HTMLSourceElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'span'): HTMLCollection<HTMLSpanElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'style'): HTMLCollection<HTMLStyleElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'textarea'): HTMLCollection<HTMLTextAreaElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'video'): HTMLCollection<HTMLVideoElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'tr'): HTMLCollection<HTMLTableRowElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: 'td' | 'th'): HTMLCollection<HTMLTableCellElement>;
  getElementsByTagNameNS(namespaceURI: string, localName: string): HTMLCollection<HTMLElement>;
  hasAttribute(name: string): boolean;
  hasAttributeNS(namespaceURI: string, localName: string): boolean;
  insertAdjacentElement(position: 'beforebegin' | 'afterbegin' | 'beforeend' | 'afterend', element: Element): void;
  insertAdjacentHTML(position: 'beforebegin' | 'afterbegin' | 'beforeend' | 'afterend', html: string): void;
  insertAdjacentText(position: 'beforebegin' | 'afterbegin' | 'beforeend' | 'afterend', text: string): void;
  matches(selector: string): bool;
  querySelector(selector: string): HTMLElement;
  querySelectorAll(selector: string): NodeList<HTMLElement>;
  releasePointerCapture(pointerId: string): void;
  remove(): void;
  removeAttribute(name?: string): void;
  removeAttributeNode(attributeNode: Attr): Attr;
  removeAttributeNS(namespaceURI: string, localName: string): void;
  requestFullscren(): void;
  requestPointerLock(): void;
  scrollIntoView(arg?: (boolean | { behavior?: ('auto' | 'instant' | 'smooth'), block?: ('start' | 'end') })): void;
  setAttribute(name?: string, value?: string): void;
  setAttributeNS(namespaceURI: string, qualifiedName: string, value: string): void;
  setAttributeNode(newAttr: Attr): Attr;
  setAttributeNodeNS(newAttr: Attr): Attr;
  setPointerCapture(pointerId: string): void;
}

declare class HTMLElement extends Element {
  blur(): void;
  click(): void;
  focus(): void;
  getBoundingClientRect(): ClientRect;
  forceSpellcheck(): void;
  accessKey: string;
  accessKeyLabel: string;
  className: string;
  contentEditable: string;
  contextMenu: ?HTMLMenuElement;
  dataset: {[key:string]: string};
  dir: 'ltr' | 'trl' | 'auto';
  draggable: bool;
  dropzone: any;
  hidden: boolean;
  id: string;
  innerHTML: string;
  isContentEditable: boolean;
  itemProp: any;
  itemScope: bool;
  itemType: any;
  itemValue: Object;
  lang: string;
  offsetHeight: number;
  offsetLeft: number;
  offsetParent: Element;
  offsetTop: number;
  offsetWidth: number;
  onabort: ?Function;
  onblur: ?Function;
  oncancel: ?Function;
  oncanplay: ?Function;
  oncanplaythrough: ?Function;
  onchange: ?Function;
  onclick: ?Function;
  oncuechange: ?Function;
  ondblclick: ?Function;
  ondurationchange: ?Function;
  onemptied: ?Function;
  onended: ?Function;
  onerror: ?Function;
  onfocus: ?Function;
  oninput: ?Function;
  oninvalid: ?Function;
  onkeydown: ?Function;
  onkeypress: ?Function;
  onkeyup: ?Function;
  onload: ?Function;
  onloadeddata: ?Function;
  onloadedmetadata: ?Function;
  onloadstart: ?Function;
  onmousedown: ?Function;
  onmouseenter: ?Function;
  onmouseleave: ?Function;
  onmousemove: ?Function;
  onmouseout: ?Function;
  onmouseover: ?Function;
  onmouseup: ?Function;
  onmousewheel: ?Function;
  onpause: ?Function;
  onplay: ?Function;
  onplaying: ?Function;
  onprogress: ?Function;
  onratechange: ?Function;
  onreadystatechange: ?Function;
  onreset: ?Function;
  onresize: ?Function;
  onscroll: ?Function;
  onseeked: ?Function;
  onseeking: ?Function;
  onselect: ?Function;
  onshow: ?Function;
  onstalled: ?Function;
  onsubmit: ?Function;
  onsuspend: ?Function;
  ontimeupdate: ?Function;
  ontoggle: ?Function;
  onvolumechange: ?Function;
  onwaiting: ?Function;
  properties: any;
  spellcheck: boolean;
  style: CSSStyleDeclaration;
  tabIndex: number;
  title: string;
  translate: boolean;
}

declare class HTMLTableCellElement extends HTMLElement {
  colSpan: number;
  rowSpan: number;
  cellIndex: number;
}

declare class HTMLTableRowElement extends HTMLElement {
  align: 'left' | 'right' | 'center';
  rowIndex: number;
  deleteCell(index: number): void;
  insertCell(index: number): HTMLTableCellElement;
}

declare class HTMLMenuElement extends HTMLElement {
  getCompact(): bool;
  setCompact(compact: bool): void;
}

declare class HTMLBaseElement extends HTMLElement {
  href: string;
  target: string;
}

declare class CanvasGradient {
  addColorStop(offset: number, color: string): void;
}

declare class CanvasPattern {
  setTransform(matrix: SVGMatrix): void;
}

declare class ImageBitmap {
  close(): void;
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

  // CanvasPathMethods
  // shared path API methods
  arc(x: number, y: number, radius: number, startAngle: number, endAngle: number, anticlockwise?: boolean): void;
  arcTo(x1: number, y1: number, x2: number, y2: number, radius: number, _: void, _: void): void;
  arcTo(x1: number, y1: number, x2: number, y2: number, radiusX: number, radiusY: number, rotation: number): void;
  bezierCurveTo(cp1x: number, cp1y: number, cp2x: number, cp2y: number, x: number, y: number): void;
  closePath(): void;
  ellipse(x: number, y: number, radiusX: number, radiusY: number, rotation: number, startAngle: number, endAngle: number, anticlockwise?: boolean): void;
  lineTo(x: number, y: number): void;
  moveTo(x: number, y: number): void;
  quadraticCurveTo(cpx: number, cpy: number, x: number, y: number): void;
  rect(x: number, y: number, w: number, h: number): void;
};

declare class ImageData {
  width: number;
  height: number;
  data: Uint8ClampedArray;
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
  getContext(contextId: "2d", ...args: any): ?CanvasRenderingContext2D;
  getContext(contextId: "webgl", ...args: any): ?WebGLRenderingContext;
  getContext(contextId: string, ...args: any): ?RenderingContext; // fallback
  toDataURL(type?: string, ...args: any): string;
  toBlob(callback: (v: File) => void, type?: string, ...args: any): void;
}

declare class HTMLFormElement extends HTMLElement {
  [name: string]: any;
  acceptCharset: string;
  action: string;
  elements: HTMLCollection<HTMLElement>;
  encoding: string;
  enctype: string;
  length: number;
  method: string;
  name: string;
  target: string;

  checkValidity(): boolean;
  item(name?: any, index?: any): any;
  namedItem(name: string): any;
  reset(): void;
  submit(): void;
}

declare class HTMLIFrameElement extends HTMLElement {
  allowFullScreen: boolean;
  contentDocument: Document;
  contentWindow: any;
  frameBorder: string;
  height: string;
  marginHeight: string;
  marginWidth: string;
  name: string;
  scrolling: string;
  sandbox: DOMTokenList;
  src: string;
  srcDoc: string;
  width: string;
}

declare class HTMLImageElement extends HTMLElement {
  alt: string;
  complete: boolean; // readonly
  crossOrigin: ?string;
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
  crossOrigin: ?string;
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

declare class HTMLAudioElement extends HTMLMediaElement {
}

declare class HTMLVideoElement extends HTMLMediaElement {
  width: number;
  height: number;
  videoWidth: number;
  videoHeight: number;
  poster: string;
}

declare class HTMLSourceElement extends HTMLElement {
    src: string;
    type: string;

    //when used with the picture element
    srcset: string;
    sizes: string;
    media: string;
}

declare class ValidityState {
  badInput: boolean;
  customError: boolean;
  patternMismatch: boolean;
  rangeOverflow: boolean;
  rangeUnderflow: boolean;
  stepMismatch: boolean;
  tooLong: boolean;
  typeMismatch: boolean;
  valueMissing: boolean;
}

// http://www.w3.org/TR/html5/forms.html#dom-textarea/input-setselectionrange
type SelectionDirection = 'backward' | 'forward' | 'none';
type SelectionMode = 'select' | 'start' | 'end' | 'preserve';
declare class HTMLInputElement extends HTMLElement {
  accept: string;
  align: string;
  alt: string;
  autocomplete: string;
  autofocus: boolean;
  border: string;
  checked: boolean;
  complete: boolean;
  defaultChecked: boolean;
  defaultValue: string;
  dirname: string;
  disabled: boolean;
  dynsrc: string;
  files: FileList;
  form: HTMLFormElement | null;
  formAction: string;
  formEncType: string;
  formMethod: string;
  formNoValidate: boolean;
  formTarget: string;
  height: string;
  hspace: number;
  indeterminate: boolean;
  labels: NodeList<HTMLLabelElement>;
  list: HTMLElement | null;
  loop: number;
  lowsrc: string;
  max: string;
  maxLength: number;
  min: string;
  multiple: boolean;
  name: string;
  pattern: string;
  placeholder: string;
  readOnly: boolean;
  required: boolean;
  selectionDirection: SelectionDirection;
  selectionEnd: number;
  selectionStart: number;
  size: number;
  src: string;
  start: string;
  status: boolean;
  step: string;
  tabIndex: number;
  type: string;
  useMap: string;
  validationMessage: string;
  validity: ValidityState;
  value: string;
  valueAsDate: Date;
  valueAsNumber: number;
  vrml: string;
  vspace: number;
  width: string;
  willValidate: boolean;

  blur(): void;
  checkValidity(): boolean;
  setCustomValidity(error: string): void;
  click(): void;
  createTextRange(): TextRange;
  focus(): void;
  select(): void;
  setRangeText(replacement: string, start?: void, end?: void, selectMode?: void): void;
  setRangeText(replacement: string, start: number, end: number, selectMode?: SelectionMode): void;
  setSelectionRange(start: number, end: number, direction?: SelectionDirection): void;
}

declare class HTMLButtonElement extends HTMLElement {
  disabled: boolean;
  form: HTMLFormElement | null;
  name: string;
  type: string;
  value: string;

  checkValidity(): boolean;
}

// http://dev.w3.org/html5/spec-preview/the-textarea-element.html
declare class HTMLTextAreaElement extends HTMLElement {
  autofocus: boolean;
  cols: number;
  dirName: string;
  disabled: boolean;
  form: HTMLFormElement | null;
  maxLength: number;
  name: string;
  placeholder: string;
  readOnly: boolean;
  required: boolean;
  rows: number;
  wrap: string;

  type: string;
  defaultValue: string;
  value: string;
  textLength: number;

  willValidate: boolean;
  validity: ValidityState;
  validationMessage: string;
  checkValidity(): boolean;
  setCustomValidity(error: string): void;

  labels: NodeList<HTMLLabelElement>;

  select(): void;
  selectionStart: number;
  selectionEnd: number;
  selectionDirection: SelectionDirection;
  setSelectionRange(start: number, end: number, direction?: SelectionDirection): void;
}

declare class HTMLSelectElement extends HTMLElement {
  disabled: boolean;
  form: HTMLFormElement | null;
  length: number;
  multiple: boolean;
  name: string;
  options: HTMLOptionsCollection;
  selectedIndex: number;
  size: number;
  type: string;
  value: string;

  add(element: HTMLElement, before?: HTMLElement): void;
  checkValidity(): boolean;
  remove(index?: number): void;
}

declare class HTMLOptionsCollection {
  length: number;
  item(index: number): Node;
  namedItem(name: string): Node;
}

declare class HTMLOptionElement extends HTMLElement {
  defaultSelected: boolean;
  disabled: boolean;
  form: HTMLFormElement | null;
  index: number;
  label: string;
  selected: boolean;
  text: string;
  value: string;
}

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
  target: string;
  text: string;
  type: string;
  username: string;
}

// http://dev.w3.org/html5/spec-preview/the-label-element.html
declare class HTMLLabelElement extends HTMLElement {
  form: HTMLFormElement | null;
  htmlFor: string;
  control: HTMLElement | null;
}

declare class HTMLLinkElement extends HTMLElement {
  crossOrigin: ?('anonymous' | 'use-credentials');
  href: string;
  hreflang: string;
  media: string;
  rel: string;
  sizes: DOMTokenList;
  type: string;
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

declare class HTMLParagraphElement extends HTMLElement {
  align: 'left' | 'center' | 'right' | 'justify'; // deprecated in HTML 4.01
}

declare class HTMLDivElement extends HTMLElement {}

declare class HTMLSpanElement extends HTMLElement {}

declare class HTMLAppletElement extends HTMLElement {}

declare class HTMLEmbedElement extends HTMLElement {}

declare class HTMLMetaElement extends HTMLElement {
  content: string;
  httpEquiv: string;
  name: string;
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
  @@iterator(): Iterator<ClientRect>;
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
  constructor(url: string, base?: string | URL): void;
  hash: string;
  host: string;
  hostname: string;
  href: string;
  origin: string; // readonly
  password: string;
  pathname: string;
  port: string;
  protocol: string;
  search: string;
  searchParams: URLSearchParams; // readonly
  username: string;
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

  appendBuffer(data: ArrayBuffer | $ArrayBufferView): void;
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

// TODO: The use of `typeof` makes this function signature effectively
// (node: Node) => number, but it should be (node: Node) => 1|2|3
type NodeFilterCallback = (node: Node) =>
typeof NodeFilter.FILTER_ACCEPT |
typeof NodeFilter.FILTER_REJECT |
typeof NodeFilter.FILTER_SKIP;

type NodeFilterInterface = NodeFilterCallback | { accept: NodeFilterCallback }

// TODO: window.NodeFilter exists at runtime and behaves as a constructor
//       as far as `instanceof` is concerned, but it is not callable.
declare class NodeFilter {
  static SHOW_ALL: -1;
  static SHOW_ELEMENT: 1;
  static SHOW_ATTRIBUTE: 2; // deprecated
  static SHOW_TEXT: 4;
  static SHOW_CDATA_SECTION: 8; // deprecated
  static SHOW_ENTITY_REFERENCE: 16; // deprecated
  static SHOW_ENTITY: 32; // deprecated
  static SHOW_PROCESSING_INSTRUCTION: 64;
  static SHOW_COMMENT: 128;
  static SHOW_DOCUMENT: 256;
  static SHOW_DOCUMENT_TYPE: 512;
  static SHOW_DOCUMENT_FRAGMENT: 1024;
  static SHOW_NOTATION: 2048; // deprecated
  static FILTER_ACCEPT: 1;
  static FILTER_REJECT: 2;
  static FILTER_SKIP: 3;
  acceptNode: NodeFilterCallback;
}

// TODO: window.NodeIterator exists at runtime and behaves as a constructor
//       as far as `instanceof` is concerned, but it is not callable.
declare class NodeIterator<RootNodeT, WhatToShowT> {
  root: RootNodeT;
  whatToShow: number;
  filter: NodeFilter;
  expandEntityReferences: boolean;
  referenceNode: RootNodeT | WhatToShowT;
  pointerBeforeReferenceNode: boolean;
  detach(): void;
  previousNode(): WhatToShowT | null;
  nextNode(): WhatToShowT | null;
}

// TODO: window.TreeWalker exists at runtime and behaves as a constructor
//       as far as `instanceof` is concerned, but it is not callable.
declare class TreeWalker<RootNodeT, WhatToShowT> {
  root: RootNodeT;
  whatToShow: number;
  filter: NodeFilter;
  expandEntityReferences: boolean;
  currentNode: RootNodeT | WhatToShowT;
  parentNode(): WhatToShowT | null;
  firstChild(): WhatToShowT | null;
  lastChild(): WhatToShowT | null;
  previousSibling(): WhatToShowT | null;
  nextSibling(): WhatToShowT | null;
  previousNode(): WhatToShowT | null;
  nextNode(): WhatToShowT | null;
}

/* window */

declare type WindowProxy = any;
declare function alert(message?: any): void;
declare function prompt(message?: any, value?: any): string;
declare function close(): void;
declare function confirm(message?: string): boolean;
declare var event: Event;
declare function getComputedStyle(elt: Element, pseudoElt?: string): any;
declare function requestAnimationFrame(callback: (timestamp: number) => void): number;
declare function cancelAnimationFrame(requestId: number): void;
declare function requestIdleCallback(
  cb: (deadline: {didTimeout: boolean, timeRemaining: () => number}) => void,
  opts?: {timeout: number},
): number;
declare function cancelIdleCallback(id: number): void;
declare var localStorage: Storage;
declare function focus(): void;
declare function onfocus(ev: Event): any;
declare function onmessage(ev: MessageEvent): any;
declare function open(url?: string, target?: string, features?: string, replace?: boolean): any;
declare var parent: WindowProxy;
declare function print(): void;
declare var self: any;
declare var sessionStorage: Storage;
declare var status: string;
declare var top: WindowProxy;
declare function getSelection(): Selection | null;
