/**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */
/* React */

/**
 * A UI node that can be rendered by React. React can render most primitives in
 * addition to elements and arrays of nodes.
 */
declare type React$Node =
  | void
  | null
  | boolean
  | number
  | string
  | React$Element<any>
  | Iterable<React$Node>;

/**
 * Base class of ES6 React classes, modeled as a polymorphic class whose main
 * type parameters are Props and State.
 */
declare class React$Component<Props, State = void> {
  // fields

  props: Props;
  state: State;

  // action methods

  setState(
    partialState: $Shape<State> | ((State, Props) => $Shape<State> | void),
    callback?: () => mixed,
  ): void;

  forceUpdate(callback?: () => void): void;

  // lifecycle methods

  constructor(props?: Props, context?: any): void;
  render(): React$Node;
  componentWillMount(): void;
  componentDidMount(): void;
  componentWillReceiveProps(
    nextProps: Props,
    nextContext: any,
  ): void;
  shouldComponentUpdate(
    nextProps: Props,
    nextState: State,
    nextContext: any,
  ): boolean;
  componentWillUpdate(
    nextProps: Props,
    nextState: State,
    nextContext: any,
  ): void;
  componentDidUpdate(
    prevProps: Props,
    prevState: State,
    prevContext: any,
  ): void;
  componentWillUnmount(): void;

  // long tail of other stuff not modeled very well

  refs: any;
  context: any;
  getChildContext(): any;
  static displayName: string;
  static childContextTypes: any;
  static contextTypes: any;
  static propTypes: $Subtype<{[_: $Keys<Props>]: any}>;

  // We don't add a type for `defaultProps` so that its type may be entirely
  // inferred when we diff the type for `defaultProps` with `Props`. Otherwise
  // the user would need to define a type (which would be redundant) to override
  // the type we provide here in the base class.
  //
  // static defaultProps: $Shape<Props>;
}

declare class React$PureComponent<Props, State = void>
  extends React$Component<Props, State> {
  // TODO: Due to bugs in Flow's handling of React.createClass, some fields
  // already declared in the base class need to be redeclared below. Ideally
  // they should simply be inherited.

  props: Props;
  state: State;
}

/**
 * Base class of legacy React classes, which extends the base class of ES6 React
 * classes and supports additional methods.
 */
declare class LegacyReactComponent<Props, State>
  extends React$Component<Props, State> {
  // additional methods

  replaceState(state: State, callback?: () => void): void;

  isMounted(): bool;

  // TODO: Due to bugs in Flow's handling of React.createClass, some fields
  // already declared in the base class need to be redeclared below. Ideally
  // they should simply be inherited.

  props: Props;
  state: State;
}

/**
 * The type of a stateless functional component. In most cases these components
 * are a single function. However, they may have some static properties that we
 * can type check.
 */
declare type React$StatelessFunctionalComponent<Props> = {
  (props: Props, context: any): React$Node,
  propTypes?: $Subtype<{[_: $Keys<Props>]: any}>,
  contextTypes?: any
};

/**
 * The type of a component in React. A React component may be a:
 *
 * - Stateless functional components. Functions that take in props as an
 *   argument and return a React node.
 * - ES6 class component. Components with state defined either using the ES6
 *   class syntax, or with the legacy `React.createClass()` helper.
 */
declare type React$ComponentType<Props> =
  | React$StatelessFunctionalComponent<Props>
  | Class<React$Component<Props, any>>;

/**
 * The type of an element in React. A React element may be a:
 *
 * - String. These elements are intrinsics that depend on the React renderer
 *   implementation.
 * - React component. See `ComponentType` for more information about its
 *   different variants.
 */
declare type React$ElementType =
  | $Keys<$JSXIntrinsics>
  | React$ComponentType<any>;

/**
 * Type of a React element. React elements are commonly created using JSX
 * literals, which desugar to React.createElement calls (see below).
 */
declare type React$Element<ElementType: React$ElementType> = {|
  +type: ElementType,
  +props: React$ElementProps<ElementType>,
  +key: React$Key | null,
  +ref: any,
|};

/**
 * The type of the key that React uses to determine where items in a new list
 * have moved.
 */
declare type React$Key = string | number;

/**
 * The type of the ref prop available on all React components.
 */
declare type React$Ref<ElementType: React$ElementType> =
  | ((React$ElementRef<ElementType> | null) => mixed)
  | string;

declare module react {
  declare export var DOM: any;
  declare export var PropTypes: ReactPropTypes;
  declare export var version: string;

  declare export function initializeTouchEvents(shouldUseTouch: boolean): void;

  declare export function checkPropTypes<V>(
    propTypes: $Subtype<{[_: $Keys<V>]: ReactPropsCheckType}>,
    values: V,
    location: string,
    componentName: string,
    getStack: ?(() => ?string)
  ) : void;

  declare export var createClass: React$CreateClass;
  declare export var createElement: React$CreateElement;
  declare export var cloneElement: React$CloneElement;
  declare export function createFactory<ElementType: React$ElementType>(
    type: ElementType,
  ): React$ElementFactory<ElementType>;

  declare export function isValidElement(element: any): boolean;

  declare export var Component: typeof React$Component;
  declare export var PureComponent: typeof React$PureComponent;
  declare export type StatelessFunctionalComponent<P> =
    React$StatelessFunctionalComponent<P>;
  declare export type ComponentType<P> = React$ComponentType<P>;
  declare export type ElementType = React$ElementType;
  declare export type Element<C> = React$Element<C>;
  declare export type Key = React$Key;
  declare export type Ref<C> = React$Ref<C>;
  declare export type Node = React$Node;

  declare export type ElementProps<C> = React$ElementProps<C>;
  declare export type ElementRef<C> = React$ElementRef<C>;

  declare export type ChildrenArray<T> = $ReadOnlyArray<ChildrenArray<T>> | T;
  declare export var Children: {
    map<T, U>(
      children: ChildrenArray<T>,
      fn: (child: $NonMaybeType<T>, index: number) => U,
      thisArg?: mixed,
    ): Array<$NonMaybeType<U>>;
    forEach<T>(
      children: ChildrenArray<T>,
      fn: (child: T, index: number) => mixed,
      thisArg?: mixed,
    ): void;
    count(children: ChildrenArray<any>): number;
    only<T>(children: ChildrenArray<T>): $NonMaybeType<T>;
    toArray<T>(children: ChildrenArray<T>): Array<$NonMaybeType<T>>;
  };

  declare export default {|
    +DOM: typeof DOM,
    +PropTypes: typeof PropTypes,
    +version: typeof version,
    +initializeTouchEvents: typeof initializeTouchEvents,
    +checkPropTypes: typeof checkPropTypes,
    +createClass: typeof createClass,
    +createElement: typeof createElement,
    +cloneElement: typeof cloneElement,
    +createFactory: typeof createFactory,
    +isValidElement: typeof isValidElement,
    +Component: typeof Component,
    +PureComponent: typeof PureComponent,
    +Children: typeof Children,
  |};
}

// TODO Delete this once https://github.com/facebook/react/pull/3031 lands
// and "react" becomes the standard name for this module
declare module React {
  declare var exports: $Exports<'react'>;
}

declare module 'react-dom' {
  declare function findDOMNode(
    componentOrElement : Element | ?React$Component<any, any>,
  ): null | Element | Text;

  declare function render<ElementType: React$ElementType>(
    element: React$Element<ElementType>,
    container: any,
    callback?: () => void,
  ): React$ElementRef<ElementType>;

  declare function unmountComponentAtNode(container: any): boolean;
  declare var version: string;

  declare function unstable_batchedUpdates<A, B, C, D, E>(
    callback: (a: A, b: B, c: C, d: D, e: E) => mixed,
    a: A,
    b: B,
    c: C,
    d: D,
    e: E
  ): void;
  declare function unstable_renderSubtreeIntoContainer<
    ElementType: React$ElementType,
  >(
    parentComponent: React$Component<any, any>,
    nextElement: React$Element<ElementType>,
    container: any,
    callback?: () => void
  ): React$ElementRef<ElementType>;
}

declare module 'react-dom/server' {
  declare function renderToString(element: React$Node): string;
  declare function renderToStaticMarkup(element: React$Node): string;
}

type ReactPropsCheckType = (
  props: any,
  propName: string,
  componentName: string,
  href?: string) => ?Error;

type ReactPropsChainableTypeChecker = {
  isRequired: ReactPropsCheckType;
  (props: any, propName: string, componentName: string, href?: string): ?Error;
};

type React$PropTypes$arrayOf =
  (typeChecker: ReactPropsCheckType) => ReactPropsChainableTypeChecker;
type React$PropTypes$instanceOf =
  (expectedClass: any) => ReactPropsChainableTypeChecker;
type React$PropTypes$objectOf =
  (typeChecker: ReactPropsCheckType) => ReactPropsChainableTypeChecker;
type React$PropTypes$oneOf =
  (expectedValues: Array<any>) => ReactPropsChainableTypeChecker;
type React$PropTypes$oneOfType =
  (arrayOfTypeCheckers: Array<ReactPropsCheckType>) =>
    ReactPropsChainableTypeChecker;
type React$PropTypes$shape =
  (shapeTypes: { [key: string]: ReactPropsCheckType }) =>
    ReactPropsChainableTypeChecker;

type ReactPropTypes = {
  array: React$PropType$Primitive<Array<any>>;
  bool: React$PropType$Primitive<boolean>;
  func: React$PropType$Primitive<Function>;
  number: React$PropType$Primitive<number>;
  object: React$PropType$Primitive<Object>;
  string: React$PropType$Primitive<string>;
  any: React$PropType$Primitive<any>;
  arrayOf: React$PropType$ArrayOf;
  element: React$PropType$Primitive<any>; /* TODO */
  instanceOf: React$PropType$InstanceOf;
  node: React$PropType$Primitive<any>; /* TODO */
  objectOf: React$PropType$ObjectOf;
  oneOf: React$PropType$OneOf;
  oneOfType: React$PropType$OneOfType;
  shape: React$PropType$Shape;
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

declare class SyntheticAnimationEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T> {
  animationName: string;
  elapsedTime: number;
  pseudoElement: string;
}

declare class SyntheticClipboardEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T> {
  clipboardData: any;
}

declare class SyntheticCompositionEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T> {
  data: any;
}

declare class SyntheticInputEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T> {
  +target: HTMLInputElement;
  data: any;
}

declare class SyntheticUIEvent<+T: EventTarget = EventTarget>
  extends SyntheticEvent<T> {
  detail: number;
  view: any;
}

declare class SyntheticFocusEvent<+T: EventTarget = EventTarget>
  extends SyntheticUIEvent<T> {
  relatedTarget: EventTarget;
}

declare class SyntheticKeyboardEvent<+T: EventTarget = EventTarget>
  extends SyntheticUIEvent<T> {
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

declare class SyntheticMouseEvent<+T: EventTarget = EventTarget>
  extends SyntheticUIEvent<T> {
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

declare class SyntheticDragEvent<+T: EventTarget = EventTarget>
  extends SyntheticMouseEvent<T> {
  dataTransfer: any;
}

declare class SyntheticWheelEvent<+T: EventTarget = EventTarget>
  extends SyntheticMouseEvent<T> {
  deltaMode: number;
  deltaX: number;
  deltaY: number;
  deltaZ: number;
}

declare class SyntheticTouchEvent<+T: EventTarget = EventTarget>
  extends SyntheticUIEvent<T> {
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
  extends SyntheticEvent<T> {
  propertyName: string;
  elapsedTime: number;
  pseudoElement: string;
}

type $JSXIntrinsics = {
  [string]: {
    instance: any,
    props: {
      children?: React$Node,
      [key: string]: any,
    },
  },
};
