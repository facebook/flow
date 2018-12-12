/**
 * Copyright (c) 2013-present, Facebook, Inc.
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
  getModifierState: any;
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
> extends SyntheticUIEvent<T, MouseEvent> {
  altKey: boolean;
  button: number;
  buttons: number;
  clientX: number;
  clientY: number;
  ctrlKey: boolean;
  getModifierState: any;
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
> extends SyntheticMouseEvent<T> {
  dataTransfer: any;
}

declare class SyntheticWheelEvent<
  +T: EventTarget = EventTarget,
> extends SyntheticMouseEvent<T> {
  deltaMode: number;
  deltaX: number;
  deltaY: number;
  deltaZ: number;
}

declare class SyntheticPointerEvent<
  +T: EventTarget = EventTarget,
> extends SyntheticMouseEvent<T> {
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
  a: {instance: HTMLAnchorElement, props: {children?: React$Node, [key: string]: any}},
  abbr: ReactDOM$HTMLElementJSXIntrinsic,
  address: ReactDOM$HTMLElementJSXIntrinsic,
  area: ReactDOM$HTMLElementJSXIntrinsic,
  article: ReactDOM$HTMLElementJSXIntrinsic,
  aside: ReactDOM$HTMLElementJSXIntrinsic,
  audio: {instance: HTMLAudioElement, props: {children?: React$Node, [key: string]: any}},
  b: ReactDOM$HTMLElementJSXIntrinsic,
  base: ReactDOM$HTMLElementJSXIntrinsic,
  bdi: ReactDOM$HTMLElementJSXIntrinsic,
  bdo: ReactDOM$HTMLElementJSXIntrinsic,
  big: ReactDOM$HTMLElementJSXIntrinsic,
  blockquote: ReactDOM$HTMLElementJSXIntrinsic,
  body: ReactDOM$HTMLElementJSXIntrinsic,
  br: {instance: HTMLBRElement, props: {children?: React$Node, [key: string]: any}},
  button: {instance: HTMLButtonElement, props: {children?: React$Node, [key: string]: any}},
  canvas: {instance: HTMLCanvasElement, props: {children?: React$Node, [key: string]: any}},
  caption: {instance: HTMLTableCaptionElement, props: {children?: React$Node, [key: string]: any}},
  cite: ReactDOM$HTMLElementJSXIntrinsic,
  code: ReactDOM$HTMLElementJSXIntrinsic,
  col: ReactDOM$HTMLElementJSXIntrinsic,
  colgroup: ReactDOM$HTMLElementJSXIntrinsic,
  data: ReactDOM$HTMLElementJSXIntrinsic,
  datalist: ReactDOM$HTMLElementJSXIntrinsic,
  dd: ReactDOM$HTMLElementJSXIntrinsic,
  del: ReactDOM$HTMLElementJSXIntrinsic,
  details: {instance: HTMLDetailsElement, props: {children?: React$Node, [key: string]: any}},
  dfn: ReactDOM$HTMLElementJSXIntrinsic,
  dialog: ReactDOM$HTMLElementJSXIntrinsic,
  div: {instance: HTMLDivElement, props: {children?: React$Node, [key: string]: any}},
  dl: {instance: HTMLDListElement, props: {children?: React$Node, [key: string]: any}},
  dt: ReactDOM$HTMLElementJSXIntrinsic,
  em: ReactDOM$HTMLElementJSXIntrinsic,
  embed: ReactDOM$HTMLElementJSXIntrinsic,
  fieldset: {instance: HTMLFieldSetElement, props: {children?: React$Node, [key: string]: any}},
  figcaption: ReactDOM$HTMLElementJSXIntrinsic,
  figure: ReactDOM$HTMLElementJSXIntrinsic,
  footer: ReactDOM$HTMLElementJSXIntrinsic,
  form: {instance: HTMLFormElement, props: {children?: React$Node, [key: string]: any}},
  h1: {instance: HTMLHeadingElement, props: {children?: React$Node, [key: string]: any}},
  h2: {instance: HTMLHeadingElement, props: {children?: React$Node, [key: string]: any}},
  h3: {instance: HTMLHeadingElement, props: {children?: React$Node, [key: string]: any}},
  h4: {instance: HTMLHeadingElement, props: {children?: React$Node, [key: string]: any}},
  h5: {instance: HTMLHeadingElement, props: {children?: React$Node, [key: string]: any}},
  h6: {instance: HTMLHeadingElement, props: {children?: React$Node, [key: string]: any}},
  head: ReactDOM$HTMLElementJSXIntrinsic,
  header: ReactDOM$HTMLElementJSXIntrinsic,
  hgroup: ReactDOM$HTMLElementJSXIntrinsic,
  hr: {instance: HTMLHRElement, props: {children?: React$Node, [key: string]: any}},
  html: ReactDOM$HTMLElementJSXIntrinsic,
  i: ReactDOM$HTMLElementJSXIntrinsic,
  iframe: {instance: HTMLIFrameElement, props: {children?: React$Node, [key: string]: any}},
  img: {instance: HTMLImageElement, props: {children?: React$Node, [key: string]: any}},
  ins: ReactDOM$HTMLElementJSXIntrinsic,
  kbd: ReactDOM$HTMLElementJSXIntrinsic,
  keygen: ReactDOM$HTMLElementJSXIntrinsic,
  label: {instance: HTMLLabelElement, props: {children?: React$Node, [key: string]: any}},
  legend: {instance: HTMLLegendElement, props: {children?: React$Node, [key: string]: any}},
  li: {instance: HTMLLIElement, props: {children?: React$Node, [key: string]: any}},
  link: {instance: HTMLLinkElement, props: {children?: React$Node, [key: string]: any}},
  main: ReactDOM$HTMLElementJSXIntrinsic,
  map: ReactDOM$HTMLElementJSXIntrinsic,
  mark: ReactDOM$HTMLElementJSXIntrinsic,
  menu: ReactDOM$HTMLElementJSXIntrinsic,
  menuitem: ReactDOM$HTMLElementJSXIntrinsic,
  meta: {instance: HTMLMetaElement, props: {children?: React$Node, [key: string]: any}},
  meter: ReactDOM$HTMLElementJSXIntrinsic,
  nav: ReactDOM$HTMLElementJSXIntrinsic,
  noscript: ReactDOM$HTMLElementJSXIntrinsic,
  object: ReactDOM$HTMLElementJSXIntrinsic,
  ol: {instance: HTMLOListElement, props: {children?: React$Node, [key: string]: any}},
  optgroup: {instance: HTMLOptGroupElement, props: {children?: React$Node, [key: string]: any}},
  option: {instance: HTMLOptionElement, props: {children?: React$Node, [key: string]: any}},
  output: ReactDOM$HTMLElementJSXIntrinsic,
  p: {instance: HTMLParagraphElement, props: {children?: React$Node, [key: string]: any}},
  param: ReactDOM$HTMLElementJSXIntrinsic,
  picture: ReactDOM$HTMLElementJSXIntrinsic,
  pre: {instance: HTMLPreElement, props: {children?: React$Node, [key: string]: any}},
  progress: ReactDOM$HTMLElementJSXIntrinsic,
  q: ReactDOM$HTMLElementJSXIntrinsic,
  rp: ReactDOM$HTMLElementJSXIntrinsic,
  rt: ReactDOM$HTMLElementJSXIntrinsic,
  ruby: ReactDOM$HTMLElementJSXIntrinsic,
  s: ReactDOM$HTMLElementJSXIntrinsic,
  samp: ReactDOM$HTMLElementJSXIntrinsic,
  script: {instance: HTMLScriptElement, props: {children?: React$Node, [key: string]: any}},
  section: ReactDOM$HTMLElementJSXIntrinsic,
  small: ReactDOM$HTMLElementJSXIntrinsic,
  source: {instance: HTMLSourceElement, props: {children?: React$Node, [key: string]: any}},
  span: {instance: HTMLSpanElement, props: {children?: React$Node, [key: string]: any}},
  strong: ReactDOM$HTMLElementJSXIntrinsic,
  style: {instance: HTMLStyleElement, props: {children?: React$Node, [key: string]: any}},
  sub: ReactDOM$HTMLElementJSXIntrinsic,
  summary: ReactDOM$HTMLElementJSXIntrinsic,
  sup: ReactDOM$HTMLElementJSXIntrinsic,
  table: {instance: HTMLTableElement, props: {children?: React$Node, [key: string]: any}},
  tbody: {instance: HTMLTableSectionElement, props: {children?: React$Node, [key: string]: any}},
  td: {instance: HTMLTableCellElement, props: {children?: React$Node, [key: string]: any}},
  tfoot: {instance: HTMLTableSectionElement, props: {children?: React$Node, [key: string]: any}},
  th: {instance: HTMLTableCellElement, props: {children?: React$Node, [key: string]: any}},
  thead: {instance: HTMLTableSectionElement, props: {children?: React$Node, [key: string]: any}},
  time: ReactDOM$HTMLElementJSXIntrinsic,
  title: ReactDOM$HTMLElementJSXIntrinsic,
  tr: {instance: HTMLTableRowElement, props: {children?: React$Node, [key: string]: any}},
  track: ReactDOM$HTMLElementJSXIntrinsic,
  u: ReactDOM$HTMLElementJSXIntrinsic,
  ul: {instance: HTMLUListElement, props: {children?: React$Node, [key: string]: any}},
  'var': ReactDOM$HTMLElementJSXIntrinsic,
  video: {instance: HTMLVideoElement, props: {children?: React$Node, [key: string]: any}},
  wbr: ReactDOM$HTMLElementJSXIntrinsic,
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
  input: {instance: HTMLInputElement, props: {children?: React$Node, [key: string]: any}},
  textarea: {instance: HTMLTextAreaElement, props: {children?: React$Node, [key: string]: any}},
  select: {instance: HTMLSelectElement, props: {children?: React$Node, [key: string]: any}},
  // Catch-all for custom elements.
  [string]: ReactDOM$HTMLElementJSXIntrinsic,
};

type ReactDOM$HTMLElementJSXIntrinsic = {
  instance: HTMLElement,
  props: {children?: React$Node, [key: string]: any},
};

type ReactDOM$SVGElementJSXIntrinsic = {
  instance: Element,
  props: {children?: React$Node, [key: string]: any},
};
