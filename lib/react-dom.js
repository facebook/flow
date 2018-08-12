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
  declare function renderToStaticNodeStream(element: React$Node): stream$Readable;
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

declare class SyntheticEvent<+T: EventTarget = EventTarget> {
  bubbles: boolean;
  cancelable: boolean;
  +currentTarget: T;
  defaultPrevented: boolean;
  eventPhase: number;
  isDefaultPrevented(): boolean;
  isPropagationStopped(): boolean;
  isTrusted: boolean;
  nativeEvent: Event;
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
> extends SyntheticEvent<T> {
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
> extends SyntheticUIEvent<T> {
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
> extends SyntheticUIEvent<T> {
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
> extends SyntheticUIEvent<T> {
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
  a: {instance: HTMLAnchorElement, props: ReactDOM$HTMLElementProps},
  abbr: ReactDOM$HTMLElementJSXIntrinsic,
  address: ReactDOM$HTMLElementJSXIntrinsic,
  area: {instance: HTMLElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  article: ReactDOM$HTMLElementJSXIntrinsic,
  aside: ReactDOM$HTMLElementJSXIntrinsic,
  audio: {instance: HTMLAudioElement, props: ReactDOM$HTMLElementProps},
  b: ReactDOM$HTMLElementJSXIntrinsic,
  base: {instance: HTMLElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  bdi: ReactDOM$HTMLElementJSXIntrinsic,
  bdo: ReactDOM$HTMLElementJSXIntrinsic,
  big: ReactDOM$HTMLElementJSXIntrinsic,
  blockquote: ReactDOM$HTMLElementJSXIntrinsic,
  body: ReactDOM$HTMLElementJSXIntrinsic,
  br: {instance: HTMLBRElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  button: {instance: HTMLButtonElement, props: ReactDOM$HTMLElementProps},
  canvas: {instance: HTMLCanvasElement, props: ReactDOM$HTMLElementProps},
  caption: {instance: HTMLTableCaptionElement, props: ReactDOM$HTMLElementProps},
  cite: ReactDOM$HTMLElementJSXIntrinsic,
  code: ReactDOM$HTMLElementJSXIntrinsic,
  col: {instance: HTMLElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  colgroup: ReactDOM$HTMLElementJSXIntrinsic,
  data: ReactDOM$HTMLElementJSXIntrinsic,
  datalist: ReactDOM$HTMLElementJSXIntrinsic,
  dd: ReactDOM$HTMLElementJSXIntrinsic,
  del: ReactDOM$HTMLElementJSXIntrinsic,
  details: {instance: HTMLDetailsElement, props: ReactDOM$HTMLElementProps},
  dfn: ReactDOM$HTMLElementJSXIntrinsic,
  dialog: ReactDOM$HTMLElementJSXIntrinsic,
  div: {instance: HTMLDivElement, props: ReactDOM$HTMLElementProps},
  dl: {instance: HTMLDListElement, props: ReactDOM$HTMLElementProps},
  dt: ReactDOM$HTMLElementJSXIntrinsic,
  em: ReactDOM$HTMLElementJSXIntrinsic,
  embed: {instance: HTMLElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  fieldset: {instance: HTMLFieldSetElement, props: ReactDOM$HTMLElementProps},
  figcaption: ReactDOM$HTMLElementJSXIntrinsic,
  figure: ReactDOM$HTMLElementJSXIntrinsic,
  footer: ReactDOM$HTMLElementJSXIntrinsic,
  form: {instance: HTMLFormElement, props: ReactDOM$HTMLElementProps},
  h1: {instance: HTMLHeadingElement, props: ReactDOM$HTMLElementProps},
  h2: {instance: HTMLHeadingElement, props: ReactDOM$HTMLElementProps},
  h3: {instance: HTMLHeadingElement, props: ReactDOM$HTMLElementProps},
  h4: {instance: HTMLHeadingElement, props: ReactDOM$HTMLElementProps},
  h5: {instance: HTMLHeadingElement, props: ReactDOM$HTMLElementProps},
  h6: {instance: HTMLHeadingElement, props: ReactDOM$HTMLElementProps},
  head: ReactDOM$HTMLElementJSXIntrinsic,
  header: ReactDOM$HTMLElementJSXIntrinsic,
  hgroup: ReactDOM$HTMLElementJSXIntrinsic,
  hr: {instance: HTMLHRElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  html: ReactDOM$HTMLElementJSXIntrinsic,
  i: ReactDOM$HTMLElementJSXIntrinsic,
  iframe: {instance: HTMLIFrameElement, props: ReactDOM$HTMLElementProps},
  img: {instance: HTMLImageElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  ins: ReactDOM$HTMLElementJSXIntrinsic,
  kbd: ReactDOM$HTMLElementJSXIntrinsic,
  keygen: {instance: HTMLElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  label: {instance: HTMLLabelElement, props: ReactDOM$HTMLElementProps},
  legend: {instance: HTMLLegendElement, props: ReactDOM$HTMLElementProps},
  li: {instance: HTMLLIElement, props: ReactDOM$HTMLElementProps},
  link: {instance: HTMLLinkElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  main: ReactDOM$HTMLElementJSXIntrinsic,
  map: ReactDOM$HTMLElementJSXIntrinsic,
  mark: ReactDOM$HTMLElementJSXIntrinsic,
  menu: ReactDOM$HTMLElementJSXIntrinsic,
  menuitem: {instance: HTMLElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  meta: {instance: HTMLMetaElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  meter: ReactDOM$HTMLElementJSXIntrinsic,
  nav: ReactDOM$HTMLElementJSXIntrinsic,
  noscript: ReactDOM$HTMLElementJSXIntrinsic,
  object: ReactDOM$HTMLElementJSXIntrinsic,
  ol: {instance: HTMLOListElement, props: ReactDOM$HTMLElementProps},
  optgroup: {instance: HTMLOptGroupElement, props: ReactDOM$HTMLElementProps},
  option: {instance: HTMLOptionElement, props: ReactDOM$HTMLElementProps},
  output: ReactDOM$HTMLElementJSXIntrinsic,
  p: {instance: HTMLParagraphElement, props: ReactDOM$HTMLElementProps},
  param: {instance: HTMLElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  picture: ReactDOM$HTMLElementJSXIntrinsic,
  pre: {instance: HTMLPreElement, props: ReactDOM$HTMLElementProps},
  progress: ReactDOM$HTMLElementJSXIntrinsic,
  q: ReactDOM$HTMLElementJSXIntrinsic,
  rp: ReactDOM$HTMLElementJSXIntrinsic,
  rt: ReactDOM$HTMLElementJSXIntrinsic,
  ruby: ReactDOM$HTMLElementJSXIntrinsic,
  s: ReactDOM$HTMLElementJSXIntrinsic,
  samp: ReactDOM$HTMLElementJSXIntrinsic,
  script: {instance: HTMLScriptElement, props: ReactDOM$HTMLElementProps},
  section: ReactDOM$HTMLElementJSXIntrinsic,
  small: ReactDOM$HTMLElementJSXIntrinsic,
  source: {instance: HTMLSourceElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  span: {instance: HTMLSpanElement, props: ReactDOM$HTMLElementProps},
  strong: ReactDOM$HTMLElementJSXIntrinsic,
  style: {instance: HTMLStyleElement, props: ReactDOM$HTMLElementProps},
  sub: ReactDOM$HTMLElementJSXIntrinsic,
  summary: ReactDOM$HTMLElementJSXIntrinsic,
  sup: ReactDOM$HTMLElementJSXIntrinsic,
  table: {instance: HTMLTableElement, props: ReactDOM$HTMLElementProps},
  tbody: {instance: HTMLTableSectionElement, props: ReactDOM$HTMLElementProps},
  td: {instance: HTMLTableCellElement, props: ReactDOM$HTMLElementProps},
  tfoot: {instance: HTMLTableSectionElement, props: ReactDOM$HTMLElementProps},
  th: {instance: HTMLTableCellElement, props: ReactDOM$HTMLElementProps},
  thead: {instance: HTMLTableSectionElement, props: ReactDOM$HTMLElementProps},
  time: ReactDOM$HTMLElementJSXIntrinsic,
  title: ReactDOM$HTMLElementJSXIntrinsic,
  tr: {instance: HTMLTableRowElement, props: ReactDOM$HTMLElementProps},
  track: {instance: HTMLElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  u: ReactDOM$HTMLElementJSXIntrinsic,
  ul: {instance: HTMLUListElement, props: ReactDOM$HTMLElementProps},
  'var': ReactDOM$HTMLElementJSXIntrinsic,
  video: {instance: HTMLVideoElement, props: ReactDOM$HTMLElementProps},
  wbr: {instance: HTMLElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
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
  input: {instance: HTMLInputElement, props: ReactDOM$VoidElementProps<ReactDOM$HTMLElementProps>},
  textarea: {instance: HTMLTextAreaElement, props: ReactDOM$HTMLElementProps},
  select: {instance: HTMLSelectElement, props: ReactDOM$HTMLElementProps},
  // Catch-all for custom elements.
  [string]: ReactDOM$HTMLElementJSXIntrinsic,
};

type ReactDOM$GlobalEventHandlers = {
  // TODO
};

type ReactDOM$ElementProps = {
  children?: React$Node,
  className?: string,
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

type ReactDOM$HTMLElementProps = ReactDOM$ElementProps & ReactDOM$GlobalEventHandlers & {
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
  translate?: $TODO$CaseInsensitive<"" | "yes" | "no">

  // TODO: HTML event props
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
  | ""
  | $TODO$CaseInsensitive<"true" | "false">
  | boolean;

type ReactDOM$Boolean<AttributeName: string = ""> =
  | ""
  | $TODO$CaseInsensitive<AttributeName>
  | boolean;

type ReactDOM$HTMLElementJSXIntrinsic = {
  instance: HTMLElement,
  props: ReactDOM$HTMLElementProps,
};

type ReactDOM$SVGElementJSXIntrinsic = {
  instance: Element,
  props: ReactDOM$SVGElementProps,
};

// TODO: Can we get an actual Flow $CaseInsensitive builtin for singleton
// strings and enums?
type $TODO$CaseInsensitive<Value: string> = Value;

type ReactDOM$Style = Object; // TODO

// NOTE: In principle this can be `number | string`, but constraining values to
// numbers might be a useful design choice.
type ReactDOM$Number = number;

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
