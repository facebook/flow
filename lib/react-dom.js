/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @nolint
 * @format
 */

declare module 'react-dom' {
  declare function findDOMNode(
    componentOrElement: Element | ?React$Component<any, any>,
  ): null | Element | Text;

  declare function render<ElementType: React$ElementType>(
    element: React$Element<ElementType>,
    container: Element,
    callback?: () => void,
  ): React$ElementRef<ElementType>;

  declare function hydrate<ElementType: React$ElementType>(
    element: React$Element<ElementType>,
    container: Element,
    callback?: () => void,
  ): React$ElementRef<ElementType>;

  declare function createPortal(
    node: React$Node,
    container: Element,
  ): React$Portal;

  declare function unmountComponentAtNode(container: any): boolean;
  declare var version: string;

  declare function unstable_batchedUpdates<A, B, C, D, E>(
    callback: (a: A, b: B, c: C, d: D, e: E) => mixed,
    a: A,
    b: B,
    c: C,
    d: D,
    e: E,
  ): void;
  declare function unstable_renderSubtreeIntoContainer<
    ElementType: React$ElementType,
  >(
    parentComponent: React$Component<any, any>,
    nextElement: React$Element<ElementType>,
    container: any,
    callback?: () => void,
  ): React$ElementRef<ElementType>;
}

declare module 'react-dom/server' {
  declare function renderToString(element: React$Node): string;
  declare function renderToStaticMarkup(element: React$Node): string;
  declare function renderToNodeStream(element: React$Node): stream$Readable;
  declare function renderToStaticNodeStream(
    element: React$Node,
  ): stream$Readable;
  declare var version: string;
}

type Thenable = {
  then(resolve: () => mixed, reject?: () => mixed): mixed,
};

declare module 'react-dom/test-utils' {
  declare var Simulate: {
    [eventName: string]: (element: Element, eventData?: Object) => void,
  };
  declare function renderIntoDocument(
    instance: React$Element<any>,
  ): React$Component<any, any>;
  declare function mockComponent(
    componentClass: React$ElementType,
    mockTagName?: string,
  ): Object;
  declare function isElement(element: React$Element<any>): boolean;
  declare function isElementOfType(
    element: React$Element<any>,
    componentClass: React$ElementType,
  ): boolean;
  declare function isDOMComponent(instance: any): boolean;
  declare function isCompositeComponent(
    instance: React$Component<any, any>,
  ): boolean;
  declare function isCompositeComponentWithType(
    instance: React$Component<any, any>,
    componentClass: React$ElementType,
  ): boolean;
  declare function findAllInRenderedTree(
    tree: React$Component<any, any>,
    test: (child: React$Component<any, any>) => boolean,
  ): Array<React$Component<any, any>>;
  declare function scryRenderedDOMComponentsWithClass(
    tree: React$Component<any, any>,
    className: string,
  ): Array<Element>;
  declare function findRenderedDOMComponentWithClass(
    tree: React$Component<any, any>,
    className: string,
  ): ?Element;
  declare function scryRenderedDOMComponentsWithTag(
    tree: React$Component<any, any>,
    tagName: string,
  ): Array<Element>;
  declare function findRenderedDOMComponentWithTag(
    tree: React$Component<any, any>,
    tagName: string,
  ): ?Element;
  declare function scryRenderedComponentsWithType(
    tree: React$Component<any, any>,
    componentClass: React$ElementType,
  ): Array<React$Component<any, any>>;
  declare function findRenderedComponentWithType(
    tree: React$Component<any, any>,
    componentClass: React$ElementType,
  ): ?React$Component<any, any>;
  declare function act<X>(callback: () => void): Thenable;
}

declare class SyntheticEvent<+T: EventTarget = EventTarget, +E: Event = Event> {
  bubbles: boolean;
  cancelable: boolean;
  +currentTarget: T;
  defaultPrevented: boolean;
  eventPhase: number;
  isDefaultPrevented(): boolean;
  isPropagationStopped(): boolean;
  isTrusted: boolean;
  nativeEvent: E;
  preventDefault(): void;
  stopPropagation(): void;
  // This should not be `T`. Use `currentTarget` instead. See:
  // https://github.com/DefinitelyTyped/DefinitelyTyped/issues/11508#issuecomment-256045682
  +target: EventTarget;
  timeStamp: number;
  type: string;
  persist(): void;
}

declare class SyntheticAnimationEvent<
  +T: EventTarget = EventTarget,
> extends SyntheticEvent<T> {
  animationName: string;
  elapsedTime: number;
  pseudoElement: string;
}

declare class SyntheticClipboardEvent<
  +T: EventTarget = EventTarget,
> extends SyntheticEvent<T> {
  clipboardData: any;
}

declare class SyntheticCompositionEvent<
  +T: EventTarget = EventTarget,
> extends SyntheticEvent<T> {
  data: any;
}

declare class SyntheticInputEvent<
  +T: EventTarget = EventTarget,
> extends SyntheticEvent<T> {
  +target: HTMLInputElement;
  data: any;
}

declare class SyntheticUIEvent<
  +T: EventTarget = EventTarget,
  +E: Event = Event,
> extends SyntheticEvent<T, E> {
  detail: number;
  view: any;
}

declare class SyntheticFocusEvent<
  +T: EventTarget = EventTarget,
> extends SyntheticUIEvent<T> {
  relatedTarget: EventTarget;
}

declare class SyntheticKeyboardEvent<
  +T: EventTarget = EventTarget,
> extends SyntheticUIEvent<T, KeyboardEvent> {
  altKey: boolean;
  charCode: number;
  ctrlKey: boolean;
  getModifierState(keyArg?: string): boolean;
  key: string;
  keyCode: number;
  locale: string;
  location: number;
  metaKey: boolean;
  repeat: boolean;
  shiftKey: boolean;
  which: number;
}

declare class SyntheticMouseEvent<
  +T: EventTarget = EventTarget,
  +E: Event = MouseEvent,
> extends SyntheticUIEvent<T, E> {
  altKey: boolean;
  button: number;
  buttons: number;
  clientX: number;
  clientY: number;
  ctrlKey: boolean;
  getModifierState(keyArg: string): boolean;
  metaKey: boolean;
  pageX: number;
  pageY: number;
  relatedTarget: EventTarget;
  screenX: number;
  screenY: number;
  shiftKey: boolean;
}

declare class SyntheticDragEvent<
  +T: EventTarget = EventTarget,
> extends SyntheticMouseEvent<T, DragEvent> {
  dataTransfer: any;
}

declare class SyntheticWheelEvent<
  +T: EventTarget = EventTarget,
> extends SyntheticMouseEvent<T, WheelEvent> {
  deltaMode: number;
  deltaX: number;
  deltaY: number;
  deltaZ: number;
}

declare class SyntheticPointerEvent<
  +T: EventTarget = EventTarget,
> extends SyntheticMouseEvent<T, PointerEvent> {
  pointerId: number;
  width: number;
  height: number;
  pressure: number;
  tangentialPressure: number;
  tiltX: number;
  tiltY: number;
  twist: number;
  pointerType: string;
  isPrimary: boolean;
}

declare class SyntheticTouchEvent<
  +T: EventTarget = EventTarget,
> extends SyntheticUIEvent<T, TouchEvent> {
  altKey: boolean;
  changedTouches: any;
  ctrlKey: boolean;
  getModifierState: any;
  metaKey: boolean;
  shiftKey: boolean;
  targetTouches: any;
  touches: any;
}

declare class SyntheticTransitionEvent<
  +T: EventTarget = EventTarget,
> extends SyntheticEvent<T> {
  propertyName: string;
  elapsedTime: number;
  pseudoElement: string;
}

// prettier-ignore
declare type $JSXIntrinsics = {
  // HTML
  a: ReactDOM$HTMLElementJSXIntrinsic<HTMLAnchorElement, ReactDOM$HTMLElementProps>,
  abbr: ReactDOM$HTMLElementJSXIntrinsic<>,
  address: ReactDOM$HTMLElementJSXIntrinsic<>,
  area: ReactDOM$HTMLElementJSXIntrinsic<HTMLElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  article: ReactDOM$HTMLElementJSXIntrinsic<>,
  aside: ReactDOM$HTMLElementJSXIntrinsic<>,
  audio: ReactDOM$HTMLElementJSXIntrinsic<HTMLAudioElement, ReactDOM$HTMLElementProps>,
  b: ReactDOM$HTMLElementJSXIntrinsic<>,
  base: ReactDOM$HTMLElementJSXIntrinsic<HTMLElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  bdi: ReactDOM$HTMLElementJSXIntrinsic<>,
  bdo: ReactDOM$HTMLElementJSXIntrinsic<>,
  big: ReactDOM$HTMLElementJSXIntrinsic<>,
  blockquote: ReactDOM$HTMLElementJSXIntrinsic<>,
  body: ReactDOM$HTMLElementJSXIntrinsic<>,
  br: ReactDOM$HTMLElementJSXIntrinsic<HTMLBRElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  button: ReactDOM$HTMLElementJSXIntrinsic<HTMLButtonElement, ReactDOM$HTMLElementProps>,
  canvas: ReactDOM$HTMLElementJSXIntrinsic<HTMLCanvasElement, ReactDOM$HTMLElementProps>,
  caption: ReactDOM$HTMLElementJSXIntrinsic<HTMLTableCaptionElement, ReactDOM$HTMLElementProps>,
  cite: ReactDOM$HTMLElementJSXIntrinsic<>,
  code: ReactDOM$HTMLElementJSXIntrinsic<>,
  col: ReactDOM$HTMLElementJSXIntrinsic<HTMLElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  colgroup: ReactDOM$HTMLElementJSXIntrinsic<>,
  data: ReactDOM$HTMLElementJSXIntrinsic<>,
  datalist: ReactDOM$HTMLElementJSXIntrinsic<>,
  dd: ReactDOM$HTMLElementJSXIntrinsic<>,
  del: ReactDOM$HTMLElementJSXIntrinsic<>,
  details: ReactDOM$HTMLElementJSXIntrinsic<HTMLDetailsElement, ReactDOM$HTMLElementProps>,
  dfn: ReactDOM$HTMLElementJSXIntrinsic<>,
  dialog: ReactDOM$HTMLElementJSXIntrinsic<>,
  div: ReactDOM$HTMLElementJSXIntrinsic<HTMLDivElement, ReactDOM$HTMLElementProps>,
  dl: ReactDOM$HTMLElementJSXIntrinsic<HTMLDListElement, ReactDOM$HTMLElementProps>,
  dt: ReactDOM$HTMLElementJSXIntrinsic<>,
  em: ReactDOM$HTMLElementJSXIntrinsic<>,
  embed: ReactDOM$HTMLElementJSXIntrinsic<HTMLElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  fieldset: ReactDOM$HTMLElementJSXIntrinsic<HTMLFieldSetElement, ReactDOM$HTMLElementProps>,
  figcaption: ReactDOM$HTMLElementJSXIntrinsic<>,
  figure: ReactDOM$HTMLElementJSXIntrinsic<>,
  footer: ReactDOM$HTMLElementJSXIntrinsic<>,
  form: ReactDOM$HTMLElementJSXIntrinsic<HTMLFormElement, ReactDOM$HTMLElementProps>,
  h1: ReactDOM$HTMLElementJSXIntrinsic<HTMLHeadingElement, ReactDOM$HTMLElementProps>,
  h2: ReactDOM$HTMLElementJSXIntrinsic<HTMLHeadingElement, ReactDOM$HTMLElementProps>,
  h3: ReactDOM$HTMLElementJSXIntrinsic<HTMLHeadingElement, ReactDOM$HTMLElementProps>,
  h4: ReactDOM$HTMLElementJSXIntrinsic<HTMLHeadingElement, ReactDOM$HTMLElementProps>,
  h5: ReactDOM$HTMLElementJSXIntrinsic<HTMLHeadingElement, ReactDOM$HTMLElementProps>,
  h6: ReactDOM$HTMLElementJSXIntrinsic<HTMLHeadingElement, ReactDOM$HTMLElementProps>,
  head: ReactDOM$HTMLElementJSXIntrinsic<>,
  header: ReactDOM$HTMLElementJSXIntrinsic<>,
  hgroup: ReactDOM$HTMLElementJSXIntrinsic<>,
  hr: ReactDOM$HTMLElementJSXIntrinsic<HTMLHRElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  html: ReactDOM$HTMLElementJSXIntrinsic<>,
  i: ReactDOM$HTMLElementJSXIntrinsic<>,
  iframe: ReactDOM$HTMLElementJSXIntrinsic<HTMLIFrameElement, ReactDOM$HTMLElementProps>,
  img: ReactDOM$HTMLElementJSXIntrinsic<HTMLImageElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  ins: ReactDOM$HTMLElementJSXIntrinsic<>,
  kbd: ReactDOM$HTMLElementJSXIntrinsic<>,
  keygen: ReactDOM$HTMLElementJSXIntrinsic<HTMLElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  label: ReactDOM$HTMLElementJSXIntrinsic<HTMLLabelElement, ReactDOM$HTMLElementProps>,
  legend: ReactDOM$HTMLElementJSXIntrinsic<HTMLLegendElement, ReactDOM$HTMLElementProps>,
  li: ReactDOM$HTMLElementJSXIntrinsic<HTMLLIElement, ReactDOM$HTMLElementProps>,
  link: ReactDOM$HTMLElementJSXIntrinsic<HTMLLinkElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  main: ReactDOM$HTMLElementJSXIntrinsic<>,
  map: ReactDOM$HTMLElementJSXIntrinsic<>,
  mark: ReactDOM$HTMLElementJSXIntrinsic<>,
  menu: ReactDOM$HTMLElementJSXIntrinsic<>,
  menuitem: ReactDOM$HTMLElementJSXIntrinsic<HTMLElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  meta: ReactDOM$HTMLElementJSXIntrinsic<HTMLMetaElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  meter: ReactDOM$HTMLElementJSXIntrinsic<>,
  nav: ReactDOM$HTMLElementJSXIntrinsic<>,
  noscript: ReactDOM$HTMLElementJSXIntrinsic<>,
  object: ReactDOM$HTMLElementJSXIntrinsic<>,
  ol: ReactDOM$HTMLElementJSXIntrinsic<HTMLOListElement, ReactDOM$HTMLElementProps>,
  optgroup: ReactDOM$HTMLElementJSXIntrinsic<HTMLOptGroupElement, ReactDOM$HTMLElementProps>,
  option: ReactDOM$HTMLElementJSXIntrinsic<HTMLOptionElement, ReactDOM$HTMLElementProps>,
  output: ReactDOM$HTMLElementJSXIntrinsic<>,
  p: ReactDOM$HTMLElementJSXIntrinsic<HTMLParagraphElement, ReactDOM$HTMLElementProps>,
  param: ReactDOM$HTMLElementJSXIntrinsic<HTMLElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  picture: ReactDOM$HTMLElementJSXIntrinsic<>,
  pre: ReactDOM$HTMLElementJSXIntrinsic<HTMLPreElement, ReactDOM$HTMLElementProps>,
  progress: ReactDOM$HTMLElementJSXIntrinsic<>,
  q: ReactDOM$HTMLElementJSXIntrinsic<>,
  rp: ReactDOM$HTMLElementJSXIntrinsic<>,
  rt: ReactDOM$HTMLElementJSXIntrinsic<>,
  ruby: ReactDOM$HTMLElementJSXIntrinsic<>,
  s: ReactDOM$HTMLElementJSXIntrinsic<>,
  samp: ReactDOM$HTMLElementJSXIntrinsic<>,
  script: ReactDOM$HTMLElementJSXIntrinsic<HTMLScriptElement, ReactDOM$HTMLElementProps>,
  section: ReactDOM$HTMLElementJSXIntrinsic<>,
  small: ReactDOM$HTMLElementJSXIntrinsic<>,
  source: ReactDOM$HTMLElementJSXIntrinsic<HTMLSourceElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  span: ReactDOM$HTMLElementJSXIntrinsic<HTMLSpanElement, ReactDOM$HTMLElementProps>,
  strong: ReactDOM$HTMLElementJSXIntrinsic<>,
  style: ReactDOM$HTMLElementJSXIntrinsic<HTMLStyleElement, ReactDOM$HTMLElementProps>,
  sub: ReactDOM$HTMLElementJSXIntrinsic<>,
  summary: ReactDOM$HTMLElementJSXIntrinsic<>,
  sup: ReactDOM$HTMLElementJSXIntrinsic<>,
  table: ReactDOM$HTMLElementJSXIntrinsic<HTMLTableElement, ReactDOM$HTMLElementProps>,
  tbody: ReactDOM$HTMLElementJSXIntrinsic<HTMLTableSectionElement, ReactDOM$HTMLElementProps>,
  td: ReactDOM$HTMLElementJSXIntrinsic<HTMLTableCellElement, ReactDOM$HTMLElementProps>,
  tfoot: ReactDOM$HTMLElementJSXIntrinsic<HTMLTableSectionElement, ReactDOM$HTMLElementProps>,
  th: ReactDOM$HTMLElementJSXIntrinsic<HTMLTableCellElement, ReactDOM$HTMLElementProps>,
  thead: ReactDOM$HTMLElementJSXIntrinsic<HTMLTableSectionElement, ReactDOM$HTMLElementProps>,
  time: ReactDOM$HTMLElementJSXIntrinsic<>,
  title: ReactDOM$HTMLElementJSXIntrinsic<>,
  tr: ReactDOM$HTMLElementJSXIntrinsic<HTMLTableRowElement, ReactDOM$HTMLElementProps>,
  track: ReactDOM$HTMLElementJSXIntrinsic<HTMLElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  u: ReactDOM$HTMLElementJSXIntrinsic<>,
  ul: ReactDOM$HTMLElementJSXIntrinsic<HTMLUListElement, ReactDOM$HTMLElementProps>,
  'var': ReactDOM$HTMLElementJSXIntrinsic<>,
  video: ReactDOM$HTMLElementJSXIntrinsic<HTMLVideoElement, ReactDOM$HTMLElementProps>,
  wbr: ReactDOM$HTMLElementJSXIntrinsic<HTMLElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  // SVG
  svg: ReactDOM$SVGElementJSXIntrinsic,
  animate: ReactDOM$SVGElementJSXIntrinsic,
  circle: ReactDOM$SVGElementJSXIntrinsic,
  defs: ReactDOM$SVGElementJSXIntrinsic,
  ellipse: ReactDOM$SVGElementJSXIntrinsic,
  g: ReactDOM$SVGElementJSXIntrinsic,
  image: ReactDOM$SVGElementJSXIntrinsic,
  line: ReactDOM$SVGElementJSXIntrinsic,
  linearGradient: ReactDOM$SVGElementJSXIntrinsic,
  mask: ReactDOM$SVGElementJSXIntrinsic,
  path: ReactDOM$SVGElementJSXIntrinsic,
  pattern: ReactDOM$SVGElementJSXIntrinsic,
  polygon: ReactDOM$SVGElementJSXIntrinsic,
  polyline: ReactDOM$SVGElementJSXIntrinsic,
  radialGradient: ReactDOM$SVGElementJSXIntrinsic,
  rect: ReactDOM$SVGElementJSXIntrinsic,
  stop: ReactDOM$SVGElementJSXIntrinsic,
  symbol: ReactDOM$SVGElementJSXIntrinsic,
  text: ReactDOM$SVGElementJSXIntrinsic,
  tspan: ReactDOM$SVGElementJSXIntrinsic,
  use: ReactDOM$SVGElementJSXIntrinsic,
  // Elements React adds extra props for.
  input: ReactDOM$HTMLElementJSXIntrinsic<HTMLInputElement, ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>>,
  textarea: ReactDOM$HTMLElementJSXIntrinsic<HTMLTextAreaElement, ReactDOM$HTMLElementProps>,
  select: ReactDOM$HTMLElementJSXIntrinsic<HTMLSelectElement, ReactDOM$HTMLElementProps>,
  // Catch-all for custom elements.
  [string]: ReactDOM$HTMLElementJSXIntrinsic<>,
};

type ReactDOM$GlobalEventHandlers<E> = {
  onAbort?: (SyntheticEvent<E>) => void,
  onAbortCapture?: (SyntheticEvent<E>) => void,
  onAnimationEnd?: (SyntheticAnimationEvent<E>) => void,
  onAnimationEndCapture?: (SyntheticAnimationEvent<E>) => void,
  onAnimationIteration?: (SyntheticAnimationEvent<E>) => void,
  onAnimationIterationCapture?: (SyntheticAnimationEvent<E>) => void,
  onAnimationStart?: (SyntheticAnimationEvent<E>) => void,
  onAnimationStartCapture?: (SyntheticAnimationEvent<E>) => void,
  onAuxClick?: (SyntheticMouseEvent<E>) => void,
  onAuxClickCapture?: (SyntheticMouseEvent<E>) => void,
  onBeforeInput?: (SyntheticInputEvent<E>) => void,
  onBeforeInputCapture?: (SyntheticInputEvent<E>) => void,
  onBlur?: (SyntheticFocusEvent<E>) => void,
  onBlurCapture?: (SyntheticFocusEvent<E>) => void,
  onCancel?: (SyntheticEvent<E>) => void,
  onCancelCapture?: (SyntheticEvent<E>) => void,
  onCanPlay?: (SyntheticEvent<E>) => void,
  onCanPlayCapture?: (SyntheticEvent<E>) => void,
  onCanPlayThrough?: (SyntheticEvent<E>) => void,
  onCanPlayThroughCapture?: (SyntheticEvent<E>) => void,
  onChange?: (SyntheticEvent<E>) => void,
  onChangeCapture?: (SyntheticEvent<E>) => void,
  onClick?: (SyntheticMouseEvent<E>) => void,
  onClickCapture?: (SyntheticMouseEvent<E>) => void,
  onClose?: (SyntheticEvent<E>) => void,
  onCloseCapture?: (SyntheticEvent<E>) => void,
  onCompositionEnd?: (SyntheticCompositionEvent<E>) => void,
  onCompositionEndCapture?: (SyntheticCompositionEvent<E>) => void,
  onCompositionStart?: (SyntheticCompositionEvent<E>) => void,
  onCompositionStartCapture?: (SyntheticCompositionEvent<E>) => void,
  onCompositionUpdate?: (SyntheticCompositionEvent<E>) => void,
  onCompositionUpdateCapture?: (SyntheticCompositionEvent<E>) => void,
  onContextMenu?: (SyntheticMouseEvent<E>) => void,
  onContextMenuCapture?: (SyntheticMouseEvent<E>) => void,
  onCopy?: (SyntheticEvent<E>) => void,
  onCopyCapture?: (SyntheticEvent<E>) => void,
  onCut?: (SyntheticEvent<E>) => void,
  onCutCapture?: (SyntheticEvent<E>) => void,
  onDoubleClick?: (SyntheticMouseEvent<E>) => void,
  onDoubleClickCapture?: (SyntheticMouseEvent<E>) => void,
  onDrag?: (SyntheticDragEvent<E>) => void,
  onDragCapture?: (SyntheticDragEvent<E>) => void,
  onDragEnd?: (SyntheticDragEvent<E>) => void,
  onDragEndCapture?: (SyntheticDragEvent<E>) => void,
  onDragEnter?: (SyntheticDragEvent<E>) => void,
  onDragEnterCapture?: (SyntheticDragEvent<E>) => void,
  onDragExit?: (SyntheticDragEvent<E>) => void,
  onDragExitCapture?: (SyntheticDragEvent<E>) => void,
  onDragLeave?: (SyntheticDragEvent<E>) => void,
  onDragLeaveCapture?: (SyntheticDragEvent<E>) => void,
  onDragOver?: (SyntheticDragEvent<E>) => void,
  onDragOverCapture?: (SyntheticDragEvent<E>) => void,
  onDragStart?: (SyntheticDragEvent<E>) => void,
  onDragStartCapture?: (SyntheticDragEvent<E>) => void,
  onDrop?: (SyntheticDragEvent<E>) => void,
  onDropCapture?: (SyntheticDragEvent<E>) => void,
  onDurationChange?: (SyntheticEvent<E>) => void,
  onDurationChangeCapture?: (SyntheticEvent<E>) => void,
  onEmptied?: (SyntheticEvent<E>) => void,
  onEmptiedCapture?: (SyntheticEvent<E>) => void,
  onEncrypted?: (SyntheticEvent<E>) => void,
  onEncryptedCapture?: (SyntheticEvent<E>) => void,
  onEnded?: (SyntheticEvent<E>) => void,
  onEndedCapture?: (SyntheticEvent<E>) => void,
  onError?: (SyntheticEvent<E>) => void,
  onErrorCapture?: (SyntheticEvent<E>) => void,
  onFocus?: (SyntheticFocusEvent<E>) => void,
  onFocusCapture?: (SyntheticFocusEvent<E>) => void,
  onGotPointerCapture?: (SyntheticPointerEvent<E>) => void,
  onGotPointerCaptureCapture?: (SyntheticPointerEvent<E>) => void,
  onInput?: (SyntheticEvent<E>) => void,
  onInputCapture?: (SyntheticEvent<E>) => void,
  onInvalid?: (SyntheticEvent<E>) => void,
  onInvalidCapture?: (SyntheticEvent<E>) => void,
  onKeyDown?: (SyntheticKeyboardEvent<E>) => void,
  onKeyDownCapture?: (SyntheticKeyboardEvent<E>) => void,
  onKeyPress?: (SyntheticKeyboardEvent<E>) => void,
  onKeyPressCapture?: (SyntheticKeyboardEvent<E>) => void,
  onKeyUp?: (SyntheticKeyboardEvent<E>) => void,
  onKeyUpCapture?: (SyntheticKeyboardEvent<E>) => void,
  onLoad?: (SyntheticEvent<E>) => void,
  onLoadCapture?: (SyntheticEvent<E>) => void,
  onLoadedData?: (SyntheticEvent<E>) => void,
  onLoadedDataCapture?: (SyntheticEvent<E>) => void,
  onLoadedMetadata?: (SyntheticEvent<E>) => void,
  onLoadedMetadataCapture?: (SyntheticEvent<E>) => void,
  onLoadStart?: (SyntheticEvent<E>) => void,
  onLoadStartCapture?: (SyntheticEvent<E>) => void,
  onLostPointerCapture?: (SyntheticPointerEvent<E>) => void,
  onLostPointerCaptureCapture?: (SyntheticPointerEvent<E>) => void,
  onMouseDown?: (SyntheticMouseEvent<E>) => void,
  onMouseDownCapture?: (SyntheticMouseEvent<E>) => void,
  onMouseEnter?: (SyntheticMouseEvent<E>) => void,
  onMouseLeave?: (SyntheticMouseEvent<E>) => void,
  onMouseMove?: (SyntheticMouseEvent<E>) => void,
  onMouseMoveCapture?: (SyntheticMouseEvent<E>) => void,
  onMouseOut?: (SyntheticMouseEvent<E>) => void,
  onMouseOutCapture?: (SyntheticMouseEvent<E>) => void,
  onMouseOver?: (SyntheticMouseEvent<E>) => void,
  onMouseOverCapture?: (SyntheticMouseEvent<E>) => void,
  onMouseUp?: (SyntheticMouseEvent<E>) => void,
  onMouseUpCapture?: (SyntheticMouseEvent<E>) => void,
  onPaste?: (SyntheticEvent<E>) => void,
  onPasteCapture?: (SyntheticEvent<E>) => void,
  onPause?: (SyntheticEvent<E>) => void,
  onPauseCapture?: (SyntheticEvent<E>) => void,
  onPlay?: (SyntheticEvent<E>) => void,
  onPlayCapture?: (SyntheticEvent<E>) => void,
  onPlaying?: (SyntheticEvent<E>) => void,
  onPlayingCapture?: (SyntheticEvent<E>) => void,
  onPointerCancel?: (SyntheticPointerEvent<E>) => void,
  onPointerCancelCapture?: (SyntheticPointerEvent<E>) => void,
  onPointerDown?: (SyntheticPointerEvent<E>) => void,
  onPointerDownCapture?: (SyntheticPointerEvent<E>) => void,
  onPointerEnter?: (SyntheticPointerEvent<E>) => void,
  onPointerLeave?: (SyntheticPointerEvent<E>) => void,
  onPointerMove?: (SyntheticPointerEvent<E>) => void,
  onPointerMoveCapture?: (SyntheticPointerEvent<E>) => void,
  onPointerOut?: (SyntheticPointerEvent<E>) => void,
  onPointerOutCapture?: (SyntheticPointerEvent<E>) => void,
  onPointerOver?: (SyntheticPointerEvent<E>) => void,
  onPointerOverCapture?: (SyntheticPointerEvent<E>) => void,
  onPointerUp?: (SyntheticPointerEvent<E>) => void,
  onPointerUpCapture?: (SyntheticPointerEvent<E>) => void,
  onProgress?: (SyntheticEvent<E>) => void,
  onProgressCapture?: (SyntheticEvent<E>) => void,
  onRateChange?: (SyntheticEvent<E>) => void,
  onRateChangeCapture?: (SyntheticEvent<E>) => void,
  onReset?: (SyntheticEvent<E>) => void,
  onResetCapture?: (SyntheticEvent<E>) => void,
  onScroll?: (SyntheticUIEvent<E>) => void,
  onScrollCapture?: (SyntheticUIEvent<E>) => void,
  onSeeked?: (SyntheticEvent<E>) => void,
  onSeekedCapture?: (SyntheticEvent<E>) => void,
  onSeeking?: (SyntheticEvent<E>) => void,
  onSeekingCapture?: (SyntheticEvent<E>) => void,
  onSelect?: (SyntheticEvent<E>) => void,
  onSelectCapture?: (SyntheticEvent<E>) => void,
  onStalled?: (SyntheticEvent<E>) => void,
  onStalledCapture?: (SyntheticEvent<E>) => void,
  onSubmit?: (SyntheticEvent<E>) => void,
  onSubmitCapture?: (SyntheticEvent<E>) => void,
  onSuspend?: (SyntheticEvent<E>) => void,
  onSuspendCapture?: (SyntheticEvent<E>) => void,
  onTimeUpdate?: (SyntheticEvent<E>) => void,
  onTimeUpdateCapture?: (SyntheticEvent<E>) => void,
  onToggle?: (SyntheticEvent<E>) => void,
  onToggleCapture?: (SyntheticEvent<E>) => void,
  onTouchCancel?: (SyntheticTouchEvent<E>) => void,
  onTouchCancelCapture?: (SyntheticTouchEvent<E>) => void,
  onTouchEnd?: (SyntheticTouchEvent<E>) => void,
  onTouchEndCapture?: (SyntheticTouchEvent<E>) => void,
  onTouchMove?: (SyntheticEvent<E>) => void,
  onTouchMoveCapture?: (SyntheticEvent<E>) => void,
  onTouchStart?: (SyntheticTouchEvent<E>) => void,
  onTouchStartCapture?: (SyntheticTouchEvent<E>) => void,
  onTouchTap?: (SyntheticUIEvent<E>) => void,
  onTouchTapCapture?: (SyntheticUIEvent<E>) => void,
  onTransitionEnd?: (SyntheticTransitionEvent<E>) => void,
  onTransitionEndCapture?: (SyntheticTransitionEvent<E>) => void,
  onVolumeChange?: (SyntheticEvent<E>) => void,
  onVolumeChangeCapture?: (SyntheticEvent<E>) => void,
  onWaiting?: (SyntheticEvent<E>) => void,
  onWaitingCapture?: (SyntheticEvent<E>) => void,
  onWheel?: (SyntheticWheelEvent<E>) => void,
  onWheelCapture?: (SyntheticWheelEvent<E>) => void,
};

type ReactDOM$ElementProps = {
  children?: React$Node,
  className?: ?string,
  dangerouslySetInnerHTML?: { __html: string },
  id?: string,
  lang?: string,
  style?: ReactDOM$Style,
  suppressHydrationWarning?: boolean,
  tabIndex?: ReactDOM$Number,

  // NOTE: There is no way we can enumerate all acceptable/standard props of DOM
  // intrinsics. If we ever got a Flow feature allowing regexes for keys, we
  // could express open-ended namespaces like data-* attributes, custom
  // attributes etc more precisely, and then maybe think about making this
  // definition complete/exact (thus detecting more classes of errors).
  //
  // For now the typing philosophy here is: be reasonably strict with the types
  // of known attributes, but lenient otherwise.

  [key: string]: any
};

type ReactDOM$HTMLElementProps = ReactDOM$ElementProps & {
  accessKey?: string,
  autoCapitalize?: $TODO$CaseInsensitive<"off" | "none" | "on" | "sentences" | "words" | "characters">,
  contentEditable?: ReactDOM$BooleanishString,
  contextMenu?: string,
  dir?: $TODO$CaseInsensitive<"ltr" | "rtl" | "LTR" | "RTL" | "auto">,
  draggable?: ReactDOM$BooleanishString,
  hidden?: ReactDOM$Boolean<"hidden">,
  is?: string,
  itemProp?: string,
  itemRef?: string,
  itemScope?: ReactDOM$Boolean<"itemScope" | "itemscope">,
  itemType?: string,
  slot?: string,
  spellCheck?: ReactDOM$BooleanishString,
  suppressContentEditableWarning?: boolean,
  title?: string,
  translate?: $TODO$CaseInsensitive<"yes" | "no">
};

type ReactDOM$SVGElementProps = ReactDOM$ElementProps & {
  xlinkActuate?: string,
  xlinkArcrole?: string,
  xlinkHref?: string,
  xlinkRole?: string,
  xlinkShow?: string,
  xlinkTitle?: string,
  xlinkType?: string,
  xmlBase?: string,
  xmlLang?: string,
  xmlSpace?: string

  // TODO: SVG presentation props
  // TODO: SVG filters props
  // TODO: SVG animation props
  // TODO: SVG event props
};

type ReactDOM$BooleanishString =
  | $TODO$CaseInsensitive<"true" | "false">
  | boolean;

type ReactDOM$Boolean<AttributeName: string> =
  | $TODO$CaseInsensitive<AttributeName>
  | boolean;

type ReactDOM$HTMLElementJSXIntrinsic<Instance = HTMLElement, Props = ReactDOM$HTMLElementProps> = {
  instance: Instance,
  props: Props & ReactDOM$GlobalEventHandlers<Instance>,
};

type ReactDOM$SVGElementJSXIntrinsic = {
  instance: Element,
  props: ReactDOM$SVGElementProps,
};

// TODO: Can we get an actual Flow $CaseInsensitive builtin for singleton
// strings and enums?
type $TODO$CaseInsensitive<Value: string> = Value;

type ReactDOM$Style = Object; // TODO

type ReactDOM$Number = number | string;

// Changes children and dangerouslySetInnerHTML in a given props type to be
// optional (if they're not already) and limits them to empty values.
// This helps model the React dev mode warning emitted when setting either of
// these props on an empty HTML element.
type ReactDOM$VoidElementProps<Props> = $Diff<
  Props,
  {
    children: any,
    dangerouslySetInnerHTML: any
  }
> & {
  children?: ?void, // Empty elements cannot have children
  dangerouslySetInnerHTML?: ?void // innerHTML cannot be set on empty elements
};
