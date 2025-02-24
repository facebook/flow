/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @nolint
 * @format
 * @flow
 */

// The react-dom, react-dom/server and react-dom/test-utils modules were moved to flow-typed.

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

declare class SyntheticAnimationEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T>
{
  animationName: string;
  elapsedTime: number;
  pseudoElement: string;
}

declare class SyntheticClipboardEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T>
{
  clipboardData: any;
}

declare class SyntheticCompositionEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T>
{
  data: any;
}

declare class SyntheticInputEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T>
{
  +target: HTMLInputElement;
  data: any;
}

declare class SyntheticUIEvent<+T: EventTarget = EventTarget, +E: Event = Event>
  extends SyntheticEvent<T, E>
{
  detail: number;
  view: any;
}

declare class SyntheticFocusEvent<+T: EventTarget = EventTarget>
  extends SyntheticUIEvent<T>
{
  relatedTarget: EventTarget;
}

declare class SyntheticKeyboardEvent<+T: EventTarget = EventTarget>
  extends SyntheticUIEvent<T, KeyboardEvent>
{
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
> extends SyntheticUIEvent<T, E>
{
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

declare class SyntheticDragEvent<+T: EventTarget = EventTarget>
  extends SyntheticMouseEvent<T, DragEvent>
{
  dataTransfer: any;
}

declare class SyntheticWheelEvent<+T: EventTarget = EventTarget>
  extends SyntheticMouseEvent<T, WheelEvent>
{
  deltaMode: number;
  deltaX: number;
  deltaY: number;
  deltaZ: number;
}

declare class SyntheticPointerEvent<+T: EventTarget = EventTarget>
  extends SyntheticMouseEvent<T, PointerEvent>
{
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

declare class SyntheticTouchEvent<+T: EventTarget = EventTarget>
  extends SyntheticUIEvent<T, TouchEvent>
{
  altKey: boolean;
  changedTouches: any;
  ctrlKey: boolean;
  getModifierState: any;
  metaKey: boolean;
  shiftKey: boolean;
  targetTouches: any;
  touches: any;
}

declare class SyntheticTransitionEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T>
{
  propertyName: string;
  elapsedTime: number;
  pseudoElement: string;
}

// prettier-ignore
declare type $JSXIntrinsics = {
  // Catch-all for custom elements.
  [string]: ReactDOM$HTMLElementJSXIntrinsic,
  // HTML
  a: {
    instance: HTMLAnchorElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  abbr: ReactDOM$HTMLElementJSXIntrinsic,
  address: ReactDOM$HTMLElementJSXIntrinsic,
  area: ReactDOM$HTMLElementJSXIntrinsic,
  article: ReactDOM$HTMLElementJSXIntrinsic,
  aside: ReactDOM$HTMLElementJSXIntrinsic,
  audio: {
    instance: HTMLAudioElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  b: ReactDOM$HTMLElementJSXIntrinsic,
  base: ReactDOM$HTMLElementJSXIntrinsic,
  bdi: ReactDOM$HTMLElementJSXIntrinsic,
  bdo: ReactDOM$HTMLElementJSXIntrinsic,
  big: ReactDOM$HTMLElementJSXIntrinsic,
  blockquote: ReactDOM$HTMLElementJSXIntrinsic,
  body: ReactDOM$HTMLElementJSXIntrinsic,
  br: {
    instance: HTMLBRElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  button: {
    instance: HTMLButtonElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  canvas: {
    instance: HTMLCanvasElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  caption: {
    instance: HTMLTableCaptionElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  cite: ReactDOM$HTMLElementJSXIntrinsic,
  code: ReactDOM$HTMLElementJSXIntrinsic,
  col: ReactDOM$HTMLElementJSXIntrinsic,
  colgroup: ReactDOM$HTMLElementJSXIntrinsic,
  data: ReactDOM$HTMLElementJSXIntrinsic,
  datalist: ReactDOM$HTMLElementJSXIntrinsic,
  dd: ReactDOM$HTMLElementJSXIntrinsic,
  del: ReactDOM$HTMLElementJSXIntrinsic,
  details: {
    instance: HTMLDetailsElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  dfn: ReactDOM$HTMLElementJSXIntrinsic,
  dialog: ReactDOM$HTMLElementJSXIntrinsic,
  div: {
    instance: HTMLDivElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  dl: {
    instance: HTMLDListElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  dt: ReactDOM$HTMLElementJSXIntrinsic,
  em: ReactDOM$HTMLElementJSXIntrinsic,
  embed: ReactDOM$HTMLElementJSXIntrinsic,
  fieldset: {
    instance: HTMLFieldSetElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  figcaption: ReactDOM$HTMLElementJSXIntrinsic,
  figure: ReactDOM$HTMLElementJSXIntrinsic,
  footer: ReactDOM$HTMLElementJSXIntrinsic,
  form: {
    instance: HTMLFormElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  h1: {
    instance: HTMLHeadingElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  h2: {
    instance: HTMLHeadingElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  h3: {
    instance: HTMLHeadingElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  h4: {
    instance: HTMLHeadingElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  h5: {
    instance: HTMLHeadingElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  h6: {
    instance: HTMLHeadingElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  head: ReactDOM$HTMLElementJSXIntrinsic,
  header: ReactDOM$HTMLElementJSXIntrinsic,
  hgroup: ReactDOM$HTMLElementJSXIntrinsic,
  hr: {
    instance: HTMLHRElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  html: ReactDOM$HTMLElementJSXIntrinsic,
  i: ReactDOM$HTMLElementJSXIntrinsic,
  iframe: {
    instance: HTMLIFrameElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  img: {
    instance: HTMLImageElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  ins: ReactDOM$HTMLElementJSXIntrinsic,
  kbd: ReactDOM$HTMLElementJSXIntrinsic,
  keygen: ReactDOM$HTMLElementJSXIntrinsic,
  label: {
    instance: HTMLLabelElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  legend: {
    instance: HTMLLegendElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  li: {
    instance: HTMLLIElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  link: {
    instance: HTMLLinkElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  main: ReactDOM$HTMLElementJSXIntrinsic,
  map: ReactDOM$HTMLElementJSXIntrinsic,
  mark: ReactDOM$HTMLElementJSXIntrinsic,
  menu: ReactDOM$HTMLElementJSXIntrinsic,
  menuitem: ReactDOM$HTMLElementJSXIntrinsic,
  meta: {
    instance: HTMLMetaElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  meter: ReactDOM$HTMLElementJSXIntrinsic,
  nav: ReactDOM$HTMLElementJSXIntrinsic,
  noscript: ReactDOM$HTMLElementJSXIntrinsic,
  object: ReactDOM$HTMLElementJSXIntrinsic,
  ol: {
    instance: HTMLOListElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  optgroup: {
    instance: HTMLOptGroupElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  option: {
    instance: HTMLOptionElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  output: ReactDOM$HTMLElementJSXIntrinsic,
  p: {
    instance: HTMLParagraphElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  param: ReactDOM$HTMLElementJSXIntrinsic,
  picture: ReactDOM$HTMLElementJSXIntrinsic,
  pre: {
    instance: HTMLPreElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  progress: ReactDOM$HTMLElementJSXIntrinsic,
  q: ReactDOM$HTMLElementJSXIntrinsic,
  rp: ReactDOM$HTMLElementJSXIntrinsic,
  rt: ReactDOM$HTMLElementJSXIntrinsic,
  ruby: ReactDOM$HTMLElementJSXIntrinsic,
  s: ReactDOM$HTMLElementJSXIntrinsic,
  samp: ReactDOM$HTMLElementJSXIntrinsic,
  script: {
    instance: HTMLScriptElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  section: ReactDOM$HTMLElementJSXIntrinsic,
  small: ReactDOM$HTMLElementJSXIntrinsic,
  source: {
    instance: HTMLSourceElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  span: {
    instance: HTMLSpanElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  strong: ReactDOM$HTMLElementJSXIntrinsic,
  style: {
    instance: HTMLStyleElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  sub: ReactDOM$HTMLElementJSXIntrinsic,
  summary: ReactDOM$HTMLElementJSXIntrinsic,
  sup: ReactDOM$HTMLElementJSXIntrinsic,
  table: {
    instance: HTMLTableElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  tbody: {
    instance: HTMLTableSectionElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  td: {
    instance: HTMLTableCellElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  tfoot: {
    instance: HTMLTableSectionElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  th: {
    instance: HTMLTableCellElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  thead: {
    instance: HTMLTableSectionElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  time: ReactDOM$HTMLElementJSXIntrinsic,
  title: ReactDOM$HTMLElementJSXIntrinsic,
  tr: {
    instance: HTMLTableRowElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  track: ReactDOM$HTMLElementJSXIntrinsic,
  u: ReactDOM$HTMLElementJSXIntrinsic,
  ul: {
    instance: HTMLUListElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  'var': ReactDOM$HTMLElementJSXIntrinsic,
  video: {
    instance: HTMLVideoElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
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
  input: {
    instance: HTMLInputElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  textarea: {
    instance: HTMLTextAreaElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  select: {
    instance: HTMLSelectElement,
    props: {
      +[key: string]: any,
      +children?: React$Node,
      ...
    },
    ...
  },
  ...
};

type ReactDOM$HTMLElementJSXIntrinsic = {
  instance: HTMLElement,
  props: {
    +[key: string]: any,
    +children?: React$Node,
    ...
  },
  ...
};

type ReactDOM$SVGElementJSXIntrinsic = {
  instance: Element,
  props: {
    +[key: string]: any,
    +children?: React$Node,
    ...
  },
  ...
};

type ReactDOM$BooleanishString = boolean | 'true' | 'false';

type ReactDOM$Style = any;

type ReactDOM$Number = number | string;

type ReactDOM$Boolean<AttributeName: string> = AttributeName | boolean;

// Adapted from https://github.com/DefinitelyTyped/DefinitelyTyped/blob/ffe46d9382e765fc0f54530b4653e57e6ef0921c/types/react/index.d.ts#L2377
// All the WAI-ARIA 1.1 attributes from https://www.w3.org/TR/wai-aria-1.1/
type ReactDOM$AriaAttributes = {|
  /** Identifies the currently active element when DOM focus is on a composite widget, textbox, group, or application. */
  'aria-activedescendant'?: ?string,
  /** Indicates whether assistive technologies will present all, or only parts of, the changed region based on the change notifications defined by the aria-relevant attribute. */
  'aria-atomic'?: ?ReactDOM$BooleanishString,
  /**
   * Indicates whether inputting text could trigger display of one or more predictions of the user's intended value for an input and specifies how predictions would be
   * presented if they are made.
   */
  'aria-autocomplete'?: ?('none' | 'inline' | 'list' | 'both'),
  /** Indicates an element is being modified and that assistive technologies MAY want to wait until the modifications are complete before exposing them to the user. */
  /**
   * Defines a string value that labels the current element, which is intended to be converted into Braille.
   * @see aria-label.
   */
  'aria-braillelabel'?: ?string,
  /**
   * Defines a human-readable, author-localized abbreviated description for the role of an element, which is intended to be converted into Braille.
   * @see aria-roledescription.
   */
  'aria-brailleroledescription'?: ?string,
  'aria-busy'?: ?ReactDOM$BooleanishString,
  /**
   * Indicates the current "checked" state of checkboxes, radio buttons, and other widgets.
   * @see aria-pressed @see aria-selected.
   */
  'aria-checked'?: ?(ReactDOM$BooleanishString | 'mixed'),
  /**
   * Defines the total number of columns in a table, grid, or treegrid.
   * @see aria-colindex.
   */
  'aria-colcount'?: ?number,
  /**
   * Defines an element's column index or position with respect to the total number of columns within a table, grid, or treegrid.
   * @see aria-colcount @see aria-colspan.
   */
  'aria-colindex'?: ?number,
  /**
   * Defines a human readable text alternative of aria-colindex.
   * @see aria-rowindextext.
   */
  'aria-colindextext'?: ?string,
  /**
   * Defines the number of columns spanned by a cell or gridcell within a table, grid, or treegrid.
   * @see aria-colindex @see aria-rowspan.
   */
  'aria-colspan'?: ?number,
  /**
   * Identifies the element (or elements) whose contents or presence are controlled by the current element.
   * @see aria-owns.
   */
  'aria-controls'?: ?string,
  /** Indicates the element that represents the current item within a container or set of related elements. */
  'aria-current'?: ?(
    | ReactDOM$BooleanishString
    | 'page'
    | 'step'
    | 'location'
    | 'date'
    | 'time'
  ),
  /**
   * Identifies the element (or elements) that describes the object.
   * @see aria-labelledby
   */
  'aria-describedby'?: ?string,
  /**
   * Defines a string value that describes or annotates the current element.
   * @see related aria-describedby.
   */
  'aria-description'?: ?string,
  /**
   * Identifies the element that provides a detailed, extended description for the object.
   * @see aria-describedby.
   */
  'aria-details'?: ?string,
  /**
   * Indicates that the element is perceivable but disabled, so it is not editable or otherwise operable.
   * @see aria-hidden @see aria-readonly.
   */
  'aria-disabled'?: ?ReactDOM$BooleanishString,
  /**
   * Indicates what functions can be performed when a dragged object is released on the drop target.
   * @deprecated in ARIA 1.1
   */
  'aria-dropeffect'?: ?(
    | 'none'
    | 'copy'
    | 'execute'
    | 'link'
    | 'move'
    | 'popup'
  ),
  /**
   * Identifies the element that provides an error message for the object.
   * @see aria-invalid @see aria-describedby.
   */
  'aria-errormessage'?: ?string,
  /** Indicates whether the element, or another grouping element it controls, is currently expanded or collapsed. */
  'aria-expanded'?: ?ReactDOM$BooleanishString,
  /**
   * Identifies the next element (or elements) in an alternate reading order of content which, at the user's discretion,
   * allows assistive technology to override the general default of reading in document source order.
   */
  'aria-flowto'?: ?string,
  /**
   * Indicates an element's "grabbed" state in a drag-and-drop operation.
   * @deprecated in ARIA 1.1
   */
  'aria-grabbed'?: ?ReactDOM$BooleanishString,
  /** Indicates the availability and type of interactive popup element, such as menu or dialog, that can be triggered by an element. */
  'aria-haspopup'?: ?(
    | ReactDOM$BooleanishString
    | 'menu'
    | 'listbox'
    | 'tree'
    | 'grid'
    | 'dialog'
  ),
  /**
   * Indicates whether the element is exposed to an accessibility API.
   * @see aria-disabled.
   */
  'aria-hidden'?: ?ReactDOM$BooleanishString,
  /**
   * Indicates the entered value does not conform to the format expected by the application.
   * @see aria-errormessage.
   */
  'aria-invalid'?: ?(ReactDOM$BooleanishString | 'grammar' | 'spelling'),
  /** Indicates keyboard shortcuts that an author has implemented to activate or give focus to an element. */
  'aria-keyshortcuts'?: ?string,
  /**
   * Defines a string value that labels the current element.
   * @see aria-labelledby.
   */
  'aria-label'?: ?string,
  /**
   * Identifies the element (or elements) that labels the current element.
   * @see aria-describedby.
   */
  'aria-labelledby'?: ?string,
  /** Defines the hierarchical level of an element within a structure. */
  'aria-level'?: ?number,
  /** Indicates that an element will be updated, and describes the types of updates the user agents, assistive technologies, and user can expect from the live region. */
  'aria-live'?: ?('off' | 'assertive' | 'polite'),
  /** Indicates whether an element is modal when displayed. */
  'aria-modal'?: ?ReactDOM$BooleanishString,
  /** Indicates whether a text box accepts multiple lines of input or only a single line. */
  'aria-multiline'?: ?ReactDOM$BooleanishString,
  /** Indicates that the user may select more than one item from the current selectable descendants. */
  'aria-multiselectable'?: ?ReactDOM$BooleanishString,
  /** Indicates whether the element's orientation is horizontal, vertical, or unknown/ambiguous. */
  'aria-orientation'?: ?('horizontal' | 'vertical'),
  /**
   * Identifies an element (or elements) in order to define a visual, functional, or contextual parent/child relationship
   * between DOM elements where the DOM hierarchy cannot be used to represent the relationship.
   * @see aria-controls.
   */
  'aria-owns'?: ?string,
  /**
   * Defines a short hint (a word or short phrase) intended to aid the user with data entry when the control has no value.
   * A hint could be a sample value or a brief description of the expected format.
   */
  'aria-placeholder'?: ?string,
  /**
   * Defines an element's number or position in the current set of listitems or treeitems. Not required if all elements in the set are present in the DOM.
   * @see aria-setsize.
   */
  'aria-posinset'?: ?number,
  /**
   * Indicates the current "pressed" state of toggle buttons.
   * @see aria-checked @see aria-selected.
   */
  'aria-pressed'?: ?(ReactDOM$BooleanishString | 'mixed'),
  /**
   * Indicates that the element is not editable, but is otherwise operable.
   * @see aria-disabled.
   */
  'aria-readonly'?: ?ReactDOM$BooleanishString,
  /**
   * Indicates what notifications the user agent will trigger when the accessibility tree within a live region is modified.
   * @see aria-atomic.
   */
  'aria-relevant'?: ?(
    | 'additions'
    | 'additions removals'
    | 'additions text'
    | 'all'
    | 'removals'
    | 'removals additions'
    | 'removals text'
    | 'text'
    | 'text additions'
    | 'text removals'
  ),
  /** Indicates that user input is required on the element before a form may be submitted. */
  'aria-required'?: ?ReactDOM$BooleanishString,
  /** Defines a human-readable, author-localized description for the role of an element. */
  'aria-roledescription'?: ?string,
  /**
   * Defines the total number of rows in a table, grid, or treegrid.
   * @see aria-rowindex.
   */
  'aria-rowcount'?: ?number,
  /**
   * Defines an element's row index or position with respect to the total number of rows within a table, grid, or treegrid.
   * @see aria-rowcount @see aria-rowspan.
   */
  'aria-rowindex'?: ?number,
  /**
   * Defines a human readable text alternative of aria-rowindex.
   * @see aria-colindextext.
   */
  'aria-rowindextext'?: ?string,
  /**
   * Defines the number of rows spanned by a cell or gridcell within a table, grid, or treegrid.
   * @see aria-rowindex @see aria-colspan.
   */
  'aria-rowspan'?: ?number,
  /**
   * Indicates the current "selected" state of various widgets.
   * @see aria-checked @see aria-pressed.
   */
  'aria-selected'?: ?ReactDOM$BooleanishString,
  /**
   * Defines the number of items in the current set of listitems or treeitems. Not required if all elements in the set are present in the DOM.
   * @see aria-posinset.
   */
  'aria-setsize'?: ?number,
  /** Indicates if items in a table or grid are sorted in ascending or descending order. */
  'aria-sort'?: ?('none' | 'ascending' | 'descending' | 'other'),
  /** Defines the maximum allowed value for a range widget. */
  'aria-valuemax'?: ?number,
  /** Defines the minimum allowed value for a range widget. */
  'aria-valuemin'?: ?number,
  /**
   * Defines the current value for a range widget.
   * @see aria-valuetext.
   */
  'aria-valuenow'?: ?number,
  /** Defines the human readable text alternative of aria-valuenow for a range widget. */
  'aria-valuetext'?: ?string,
|};

type ReactDOM$EventHandlers<-E> = $ReadOnly<{|
  // Clipboard Events
  onCopy?: ?(SyntheticEvent<E>) => mixed,
  onCopyCapture?: ?(SyntheticEvent<E>) => mixed,
  onCut?: ?(SyntheticEvent<E>) => mixed,
  onCutCapture?: ?(SyntheticEvent<E>) => mixed,
  onPaste?: ?(SyntheticEvent<E>) => mixed,
  onPasteCapture?: ?(SyntheticEvent<E>) => mixed,

  // Composition Events
  onCompositionEnd?: ?(SyntheticCompositionEvent<E>) => mixed,
  onCompositionEndCapture?: ?(SyntheticCompositionEvent<E>) => mixed,
  onCompositionStart?: ?(SyntheticCompositionEvent<E>) => mixed,
  onCompositionStartCapture?: ?(SyntheticCompositionEvent<E>) => mixed,
  onCompositionUpdate?: ?(SyntheticCompositionEvent<E>) => mixed,
  onCompositionUpdateCapture?: ?(SyntheticCompositionEvent<E>) => mixed,

  // Focus Events
  onFocus?: ?(SyntheticFocusEvent<E>) => mixed,
  onFocusCapture?: ?(SyntheticFocusEvent<E>) => mixed,
  onBlur?: ?(SyntheticFocusEvent<E>) => mixed,
  onBlurCapture?: ?(SyntheticFocusEvent<E>) => mixed,

  // Form Events
  onBeforeInput?: ?(SyntheticInputEvent<E>) => mixed,
  onBeforeInputCapture?: ?(SyntheticInputEvent<E>) => mixed,
  onChange?: ?(SyntheticEvent<E>) => mixed,
  onChangeCapture?: ?(SyntheticEvent<E>) => mixed,
  onInput?: ?(SyntheticEvent<E>) => mixed,
  onInputCapture?: ?(SyntheticEvent<E>) => mixed,
  onInvalid?: ?(SyntheticEvent<E>) => mixed,
  onInvalidCapture?: ?(SyntheticEvent<E>) => mixed,
  onReset?: ?(SyntheticEvent<E>) => mixed,
  onResetCapture?: ?(SyntheticEvent<E>) => mixed,
  onSubmit?: ?(SyntheticEvent<E>) => mixed,
  onSubmitCapture?: ?(SyntheticEvent<E>) => mixed,

  // Image Events
  onLoad?: ?(SyntheticEvent<E>) => mixed,
  onLoadCapture?: ?(SyntheticEvent<E>) => mixed,
  onError?: ?(SyntheticEvent<E>) => mixed,
  onErrorCapture?: ?(SyntheticEvent<E>) => mixed,

  // Keyboard Events
  onKeyDown?: ?(SyntheticKeyboardEvent<E>) => mixed,
  onKeyDownCapture?: ?(SyntheticKeyboardEvent<E>) => mixed,
  onKeyPress?: ?(SyntheticKeyboardEvent<E>) => mixed,
  onKeyPressCapture?: ?(SyntheticKeyboardEvent<E>) => mixed,
  onKeyUp?: ?(SyntheticKeyboardEvent<E>) => mixed,
  onKeyUpCapture?: ?(SyntheticKeyboardEvent<E>) => mixed,

  // Media Events
  onAbort?: ?(SyntheticEvent<E>) => mixed,
  onAbortCapture?: ?(SyntheticEvent<E>) => mixed,
  onCanPlay?: ?(SyntheticEvent<E>) => mixed,
  onCanPlayCapture?: ?(SyntheticEvent<E>) => mixed,
  onCanPlayThrough?: ?(SyntheticEvent<E>) => mixed,
  onCanPlayThroughCapture?: ?(SyntheticEvent<E>) => mixed,
  onDurationChange?: ?(SyntheticEvent<E>) => mixed,
  onDurationChangeCapture?: ?(SyntheticEvent<E>) => mixed,
  onEmptied?: ?(SyntheticEvent<E>) => mixed,
  onEmptiedCapture?: ?(SyntheticEvent<E>) => mixed,
  onEncrypted?: ?(SyntheticEvent<E>) => mixed,
  onEncryptedCapture?: ?(SyntheticEvent<E>) => mixed,
  onEnded?: ?(SyntheticEvent<E>) => mixed,
  onEndedCapture?: ?(SyntheticEvent<E>) => mixed,
  onLoadedData?: ?(SyntheticEvent<E>) => mixed,
  onLoadedDataCapture?: ?(SyntheticEvent<E>) => mixed,
  onLoadedMetadata?: ?(SyntheticEvent<E>) => mixed,
  onLoadedMetadataCapture?: ?(SyntheticEvent<E>) => mixed,
  onLoadStart?: ?(SyntheticEvent<E>) => mixed,
  onLoadStartCapture?: ?(SyntheticEvent<E>) => mixed,
  onPause?: ?(SyntheticEvent<E>) => mixed,
  onPauseCapture?: ?(SyntheticEvent<E>) => mixed,
  onPlay?: ?(SyntheticEvent<E>) => mixed,
  onPlayCapture?: ?(SyntheticEvent<E>) => mixed,
  onPlaying?: ?(SyntheticEvent<E>) => mixed,
  onPlayingCapture?: ?(SyntheticEvent<E>) => mixed,
  onProgress?: ?(SyntheticEvent<E>) => mixed,
  onProgressCapture?: ?(SyntheticEvent<E>) => mixed,
  onRateChange?: ?(SyntheticEvent<E>) => mixed,
  onRateChangeCapture?: ?(SyntheticEvent<E>) => mixed,
  onResize?: ?(SyntheticEvent<E>) => mixed,
  onResizeCapture?: ?(SyntheticEvent<E>) => mixed,
  onSeeked?: ?(SyntheticEvent<E>) => mixed,
  onSeekedCapture?: ?(SyntheticEvent<E>) => mixed,
  onSeeking?: ?(SyntheticEvent<E>) => mixed,
  onSeekingCapture?: ?(SyntheticEvent<E>) => mixed,
  onStalled?: ?(SyntheticEvent<E>) => mixed,
  onStalledCapture?: ?(SyntheticEvent<E>) => mixed,
  onSuspend?: ?(SyntheticEvent<E>) => mixed,
  onSuspendCapture?: ?(SyntheticEvent<E>) => mixed,
  onTimeUpdate?: ?(SyntheticEvent<E>) => mixed,
  onTimeUpdateCapture?: ?(SyntheticEvent<E>) => mixed,
  onVolumeChange?: ?(SyntheticEvent<E>) => mixed,
  onVolumeChangeCapture?: ?(SyntheticEvent<E>) => mixed,
  onWaiting?: ?(SyntheticEvent<E>) => mixed,
  onWaitingCapture?: ?(SyntheticEvent<E>) => mixed,

  // Mouse Events
  onAuxClick?: ?(SyntheticMouseEvent<E>) => mixed,
  onAuxClickCapture?: ?(SyntheticMouseEvent<E>) => mixed,
  onClick?: ?(SyntheticMouseEvent<E>) => mixed,
  onClickCapture?: ?(SyntheticMouseEvent<E>) => mixed,
  onContextMenu?: ?(SyntheticMouseEvent<E>) => mixed,
  onContextMenuCapture?: ?(SyntheticMouseEvent<E>) => mixed,
  onDoubleClick?: ?(SyntheticMouseEvent<E>) => mixed,
  onDoubleClickCapture?: ?(SyntheticMouseEvent<E>) => mixed,
  onDrag?: ?(SyntheticDragEvent<E>) => mixed,
  onDragCapture?: ?(SyntheticDragEvent<E>) => mixed,
  onDragEnd?: ?(SyntheticDragEvent<E>) => mixed,
  onDragEndCapture?: ?(SyntheticDragEvent<E>) => mixed,
  onDragEnter?: ?(SyntheticDragEvent<E>) => mixed,
  onDragEnterCapture?: ?(SyntheticDragEvent<E>) => mixed,
  onDragExit?: ?(SyntheticDragEvent<E>) => mixed,
  onDragExitCapture?: ?(SyntheticDragEvent<E>) => mixed,
  onDragLeave?: ?(SyntheticDragEvent<E>) => mixed,
  onDragLeaveCapture?: ?(SyntheticDragEvent<E>) => mixed,
  onDragOver?: ?(SyntheticDragEvent<E>) => mixed,
  onDragOverCapture?: ?(SyntheticDragEvent<E>) => mixed,
  onDragStart?: ?(SyntheticDragEvent<E>) => mixed,
  onDragStartCapture?: ?(SyntheticDragEvent<E>) => mixed,
  onDrop?: ?(SyntheticDragEvent<E>) => mixed,
  onDropCapture?: ?(SyntheticDragEvent<E>) => mixed,
  onMouseDown?: ?(SyntheticMouseEvent<E>) => mixed,
  onMouseDownCapture?: ?(SyntheticMouseEvent<E>) => mixed,
  onMouseEnter?: ?(SyntheticMouseEvent<E>) => mixed,
  onMouseLeave?: ?(SyntheticMouseEvent<E>) => mixed,
  onMouseMove?: ?(SyntheticMouseEvent<E>) => mixed,
  onMouseMoveCapture?: ?(SyntheticMouseEvent<E>) => mixed,
  onMouseOut?: ?(SyntheticMouseEvent<E>) => mixed,
  onMouseOutCapture?: ?(SyntheticMouseEvent<E>) => mixed,
  onMouseOver?: ?(SyntheticMouseEvent<E>) => mixed,
  onMouseOverCapture?: ?(SyntheticMouseEvent<E>) => mixed,
  onMouseUp?: ?(SyntheticMouseEvent<E>) => mixed,
  onMouseUpCapture?: ?(SyntheticMouseEvent<E>) => mixed,

  // Selection Events
  onSelect?: ?(SyntheticEvent<E>) => mixed,
  onSelectCapture?: ?(SyntheticEvent<E>) => mixed,

  // Touch Events
  onTouchCancel?: ?(SyntheticTouchEvent<E>) => mixed,
  onTouchCancelCapture?: ?(SyntheticTouchEvent<E>) => mixed,
  onTouchEnd?: ?(SyntheticTouchEvent<E>) => mixed,
  onTouchEndCapture?: ?(SyntheticTouchEvent<E>) => mixed,
  onTouchMove?: ?(SyntheticEvent<E>) => mixed,
  onTouchMoveCapture?: ?(SyntheticEvent<E>) => mixed,
  onTouchStart?: ?(SyntheticTouchEvent<E>) => mixed,
  onTouchStartCapture?: ?(SyntheticTouchEvent<E>) => mixed,

  // Pointer Events
  onPointerCancel?: ?(SyntheticPointerEvent<E>) => mixed,
  onPointerCancelCapture?: ?(SyntheticPointerEvent<E>) => mixed,
  onPointerDown?: ?(SyntheticPointerEvent<E>) => mixed,
  onPointerDownCapture?: ?(SyntheticPointerEvent<E>) => mixed,
  onPointerEnter?: ?(SyntheticPointerEvent<E>) => mixed,
  onPointerLeave?: ?(SyntheticPointerEvent<E>) => mixed,
  onPointerMove?: ?(SyntheticPointerEvent<E>) => mixed,
  onPointerMoveCapture?: ?(SyntheticPointerEvent<E>) => mixed,
  onPointerOut?: ?(SyntheticPointerEvent<E>) => mixed,
  onPointerOutCapture?: ?(SyntheticPointerEvent<E>) => mixed,
  onPointerOver?: ?(SyntheticPointerEvent<E>) => mixed,
  onPointerOverCapture?: ?(SyntheticPointerEvent<E>) => mixed,
  onPointerUp?: ?(SyntheticPointerEvent<E>) => mixed,
  onPointerUpCapture?: ?(SyntheticPointerEvent<E>) => mixed,
  onGotPointerCapture?: ?(SyntheticPointerEvent<E>) => mixed,
  onGotPointerCaptureCapture?: ?(SyntheticPointerEvent<E>) => mixed,
  onLostPointerCapture?: ?(SyntheticPointerEvent<E>) => mixed,
  onLostPointerCaptureCapture?: ?(SyntheticPointerEvent<E>) => mixed,

  // UI Events
  onScroll?: ?(SyntheticUIEvent<E>) => mixed,
  onScrollCapture?: ?(SyntheticUIEvent<E>) => mixed,

  // Wheel Events
  onWheel?: ?(SyntheticWheelEvent<E>) => mixed,
  onWheelCapture?: ?(SyntheticWheelEvent<E>) => mixed,

  // Animation Events
  onAnimationEnd?: ?(SyntheticAnimationEvent<E>) => mixed,
  onAnimationEndCapture?: ?(SyntheticAnimationEvent<E>) => mixed,
  onAnimationIteration?: ?(SyntheticAnimationEvent<E>) => mixed,
  onAnimationIterationCapture?: ?(SyntheticAnimationEvent<E>) => mixed,
  onAnimationStart?: ?(SyntheticAnimationEvent<E>) => mixed,
  onAnimationStartCapture?: ?(SyntheticAnimationEvent<E>) => mixed,

  // Toggle Events
  onToggle?: ?(SyntheticEvent<E>) => mixed,
  onToggleCapture?: ?(SyntheticEvent<E>) => mixed,

  // Transition Events
  onTransitionCancel?: ?(SyntheticTransitionEvent<E>) => mixed,
  onTransitionCancelCapture?: ?(SyntheticTransitionEvent<E>) => mixed,
  onTransitionEnd?: ?(SyntheticTransitionEvent<E>) => mixed,
  onTransitionEndCapture?: ?(SyntheticTransitionEvent<E>) => mixed,
  onTransitionRun?: ?(SyntheticTransitionEvent<E>) => mixed,
  onTransitionRunCapture?: ?(SyntheticTransitionEvent<E>) => mixed,
  onTransitionStart?: ?(SyntheticTransitionEvent<E>) => mixed,
  onTransitionStartCapture?: ?(SyntheticTransitionEvent<E>) => mixed,
|}>;

// Special props provided by React
type ReactDOM$ReactSpecificProps = {|
  children?: React$Node,
  dangerouslySetInnerHTML?: {|
    __html: string,
  |},
  defaultChecked?: ?boolean,
  defaultValue?: ?(string | number | $ReadOnlyArray<string>),
  suppressContentEditableWarning?: ?boolean,
  suppressHydrationWarning?: ?boolean,
|};

type ReactDOM$HTMLElementProps = {|
  ...ReactDOM$ReactSpecificProps,
  ...ReactDOM$AriaAttributes,
  accessKey?: ?string,
  autoCapitalize?: ?(
    | 'off'
    | 'none'
    | 'on'
    | 'sentences'
    | 'words'
    | 'characters'
  ),
  autoCorrect?: ?('off' | 'on'),
  autoFocus?: ?boolean,
  className?: ?string,
  contentEditable?: ?ReactDOM$BooleanishString,
  contextMenu?: ?string,
  dir?: ?('ltr' | 'rtl' | 'LTR' | 'RTL' | 'auto'),
  draggable?: ?ReactDOM$BooleanishString,
  enterKeyHint?: ?(
    | 'enter'
    | 'done'
    | 'go'
    | 'next'
    | 'previous'
    | 'search'
    | 'send'
  ),
  hidden?: ?ReactDOM$Boolean<'hidden'>,
  id?: ?string,
  inert?: ?ReactDOM$BooleanishString,
  inputMode?: ?(
    | 'none'
    | 'text'
    | 'tel'
    | 'url'
    | 'email'
    | 'numeric'
    | 'decimal'
    | 'search'
  ),
  is?: ?string,
  itemID?: ?string,
  itemProp?: ?string,
  itemRef?: ?string,
  itemScope?: ReactDOM$Boolean<'itemScope' | 'itemscope'>,
  itemType?: ?string,
  lang?: ?string,
  nonce?: ?string,
  popover?: ?('' | 'auto' | 'manual'),
  role?: ?string,
  slot?: ?string,
  spellCheck?: ?ReactDOM$BooleanishString,
  style?: ?ReactDOM$Style,
  suppressContentEditableWarning?: ?boolean,
  tabIndex?: ?number,
  title?: ?string,
  translate?: ?('yes' | 'no'),
|};

// Self closing tags, like br, do not allow children
type ReactDOM$SelfClosingHTMLElementProps = Omit<
  ReactDOM$HTMLElementProps,
  'children' | 'dangerouslySetInnerHTML',
>;

type ReactDOM$HTMLElementJSXIntrinsicTyped<Props, Instance = HTMLElement> = {|
  instance: Instance,
  props: {
    ...Props,
    ...ReactDOM$EventHandlers<Instance>,
    // We add data props here to avoid spreading errors
    [StringPrefix<'data-'>]: ?(string | boolean | number),
  },
|};
