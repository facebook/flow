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
 * Base class of ES6 React classes, modeled as a polymorphic class whose type
 * parameters are DefaultProps, Props, State.
 */
declare class ReactComponent<DefaultProps, Props, State> {
  // fields

  static defaultProps: $Abstract<DefaultProps>;
  props: Props;
  state: $Abstract<State>;

  // action methods

  // TODO: partialState could be a function as well
  setState(partialState: $Shape<State>, callback?: () => void): void;

  forceUpdate(callback?: () => void): void;

  // lifecycle methods

  render(): ?ReactElement<any, any, any>;
  componentWillMount(): void;
  componentDidMount(component?: any): void;
  componentWillReceiveProps(nextProps: Props, nextContext?: any): void;
  shouldComponentUpdate(nextProps: Props, nextState: State, nextContext: any): boolean;
  componentWillUpdate(nextProps: Props, nextState: State, nextContext: any): void;
  componentDidUpdate(nextProps: Props, nextState: State, nextContext: any, component: any): void;
  componentWillUnmount(): void;

  // long tail of other stuff not modeled very well

  refs: any;
  context: any;
  getChildContext(): any;
  static displayName: string;
  static childContextTypes: any;
  static contextTypes: any;
  static propTypes: $Subtype<{[_: $Keys<Props>]: any}>; //object whose keys are in PropTypes
}

/**
 * Base class of legacy React classes, which extends the base class of ES6 React
 * classes and supports additional methods.
 */
declare class LegacyReactComponent<DefaultProps, Props, State> extends ReactComponent<DefaultProps, Props, State> {
  // additional methods

  getDefaultProps(): State;
  getInitialState(): State;

  replaceState(state: State, callback?: () => void): void;

  isMounted(): bool;

  // TODO: due to bugs in Flow's handling of React.createClass, some fields
  // already declared in the base class need to be redeclared below; ideally
  // they should simply be inherited

  static defaultProps: DefaultProps;
  props: Props;
  state: State;

  static propTypes: $Subtype<{[_: $Keys<Props>]: any}>; //object whose keys are in PropTypes
  static contextTypes: any;
  static childContextTypes: any;
  static displayName: string;
}

/**
 * Type of a React class (not to be confused with the type of instances of a
 * React class, which is the React class itself). A React class is any subclass
 * of ReactComponent. We make the type of a React class polymorphic over some of
 * the type parameters (DefaultProps, Props) of ReactComponent, abstracting away
 * others (State) that are only useful for checking definitions, but not uses,
 * of a React class. The required constraints are set up using a "helper" type
 * alias, that takes an additional type parameter C representing the React
 * class, which is then abstracted with an existential type (*). The * can be
 * thought of as an "auto" instruction to the typechecker, telling it to fill in
 * the type from context.
 */
type ReactClass<DefaultProps, Props, State_DEPRECATED> = _ReactClass<DefaultProps, Props, *>;
type _ReactClass<DefaultProps, Props, C: ReactComponent<DefaultProps, Props, any>> = Class<C>;

/**
 * Type of a React element. React elements are commonly created using JSX
 * literals, which desugar to React.createElement calls (see below). Like the
 * type of a React class, the type of a React element is parametric in some but
 * not all the type parameters of ReactComponent (see above).
 */
declare class ReactElement<DefaultProps, Props, State_DEPRECATED> {
  type: ReactClass<DefaultProps, Props, any>;
  props: Props;
  key: ?string;
  ref: any;
}

type ReactPropsCheckType = (
  props:any,
  propName: string,
  componentName: string,
  href?: string) => ?Error;

type ReactPropsChainableTypeChecker = {
  isRequired: ReactPropsCheckType;
  (props:any, propName: string, componentName: string, href?: string): ?Error;
};

type ReactPropTypes = {
  array: ReactPropsChainableTypeChecker;
  bool: ReactPropsChainableTypeChecker;
  func: ReactPropsChainableTypeChecker;
  number: ReactPropsChainableTypeChecker;
  object: ReactPropsChainableTypeChecker;
  string: ReactPropsChainableTypeChecker;

  any: ReactPropsChainableTypeChecker;
  arrayOf: (typeChecker: ReactPropsCheckType) => ReactPropsChainableTypeChecker;
  element: ReactPropsChainableTypeChecker;
  instanceOf: (expectedClass: any) => ReactPropsChainableTypeChecker;
  node: ReactPropsChainableTypeChecker;
  objectOf: (typeChecker: ReactPropsCheckType) => ReactPropsChainableTypeChecker;
  oneOf: (expectedValues: Array<any>) => ReactPropsChainableTypeChecker;
  oneOfType: (arrayOfTypeCheckers: Array<ReactPropsCheckType>) => ReactPropsChainableTypeChecker;
  shape: (shapeTypes: { [key: string]: ReactPropsCheckType }) => ReactPropsChainableTypeChecker;
}

declare module react {
  declare var Children: any;
  declare var DOM: any;
  declare var PropTypes: ReactPropTypes;
  declare var version: string;

  declare function initializeTouchEvents(shouldUseTouch: boolean): void;

  // compiler magic
  declare function createClass(spec: any): ReactClass<any, any, any>;

  declare function cloneElement<DefaultProps, Props> (
    element: ReactElement<DefaultProps, Props, any>,
    attributes: $Shape<Props>,
    children?: any
  ): ReactElement<DefaultProps, Props, any>;

  /**
   * Methods that take an `attributes` argument of type Attributes, describing
   * objects whose properties must cover (at least) the difference between the
   * properties in Props and the properties in DefaultProps.  This is intended
   * to model what happens at run time: the attributes are merged with the
   * default props to obtain the props of a ReactElement / ReactComponent
   * instance.
   */
  // TODO: React DOM elements
  declare function createElement<DefaultProps, Props, Attributes: $Diff<Props, DefaultProps>>(
    name: ReactClass<DefaultProps, Props, any>,
    attributes: Attributes,
    children?: any
  ): ReactElement<DefaultProps, Props, any>;

  // TODO: React DOM elements
  declare function createFactory<DefaultProps, Props, Attributes: $Diff<Props, DefaultProps>>(
    name: ReactClass<DefaultProps, Props, any>
  ): (attributes: Attributes, children?: any) => ReactElement<DefaultProps, Props, any>;

  // TODO: React DOM elements
  declare function constructAndRenderComponent<DefaultProps, Props, Attributes: $Diff<Props, DefaultProps>>(
    name: ReactClass<DefaultProps, Props, any>,
    attributes: Attributes,
    container: any
  ): ReactComponent<DefaultProps, Props, any>;

  // TODO: React DOM elements
  declare function constructAndRenderComponentByID<DefaultProps, Props, Attributes: $Diff<Props, DefaultProps>>(
    name: ReactClass<DefaultProps, Props, any>,
    attributes: Attributes,
    id: string
  ): ReactComponent<DefaultProps, Props, any>;

  declare function findDOMNode(
    object: ReactComponent<any, any, any> | HTMLElement
  ): any;

  declare function render<DefaultProps, Props>(
    element: ReactElement<DefaultProps, Props, any>,
    container: any
  ): ReactComponent<DefaultProps, Props, any>;

  declare function renderToString(
    element: ReactElement<any, any, any>
  ): string;
  declare function renderToStaticMarkup(
    element: ReactElement<any, any, any>
  ): string;

  declare function unmountComponentAtNode(container: any): boolean;

  declare function isValidElement(element: any): boolean;
  declare function withContext(context: any, callback: () => void): any;

  declare var Component: typeof ReactComponent;
  declare var Element: typeof ReactElement;
}

// TODO Delete this once https://github.com/facebook/react/pull/3031 lands
// and "react" becomes the standard name for this module
declare module React {
    declare var exports: $Exports<'react'>;
}

// TODO: Delete the corresponding method defs from the 'react' module once React
// 0.15 comes out and removes them.
declare module 'react-dom' {
  declare function findDOMNode(
    object: ReactComponent<any, any, any> | HTMLElement
  ): any;

  declare function render<DefaultProps, Props>(
    element: ReactElement<DefaultProps, Props, any>,
    container: any
  ): ReactComponent<DefaultProps, Props, any>;

  declare function unmountComponentAtNode(container: any): boolean;
}

// TODO: Delete the corresponding method defs from the 'react' module once React
// 0.15 comes out and removes them.
declare module 'react-dom/server' {
  declare function renderToString(
    element: ReactElement<any, any, any>
  ): string;
  declare function renderToStaticMarkup(
    element: ReactElement<any, any, any>
  ): string;
}

declare class SyntheticEvent {
    bubbles: boolean;
    cancelable: boolean;
    currentTarget: EventTarget;
    defaultPrevented: boolean;
    eventPhase: number;
    isDefaultPrevented(): boolean;
    isPropagationStopped(): boolean;
    isTrusted: boolean;
    nativeEvent: Event;
    preventDefault(): void;
    stopPropagation(): void;
    target: EventTarget;
    timeStamp: number;
    type: string;
}

declare class SyntheticClipboardEvent extends SyntheticEvent {
    clipboardData: any;
}

declare class SyntheticCompositionEvent extends SyntheticEvent {
    data: any;
}

declare class SyntheticInputEvent extends SyntheticEvent {
    data: any;
}

declare class SyntheticUIEvent extends SyntheticEvent {
    detail: number;
    view: any;
}

declare class SyntheticFocusEvent extends SyntheticUIEvent {
    relatedTarget: EventTarget;
}

declare class SyntheticKeyboardEvent extends SyntheticUIEvent {
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

declare class SyntheticMouseEvent extends SyntheticUIEvent {
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

declare class SyntheticDragEvent extends SyntheticMouseEvent {
    dataTransfer: any;
}

declare class SyntheticWheelEvent extends SyntheticMouseEvent {
    deltaMode: number;
    deltaX: number;
    deltaY: number;
    deltaZ: number;
}

declare class SyntheticTouchEvent extends SyntheticUIEvent {
    altKey: boolean;
    changedTouches: any;
    ctrlKey: boolean;
    getModifierState: any;
    metaKey: boolean;
    shiftKey: boolean;
    targetTouches: any;
    touches: any;
}
