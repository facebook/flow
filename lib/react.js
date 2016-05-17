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
declare class React$Component<DefaultProps, Props, State> {
  // fields

  static defaultProps: $Abstract<DefaultProps>;
  props: Props;
  state: $Abstract<State>;

  // action methods

  // TODO: partialState could be a function as well
  setState(partialState: $Shape<State>, callback?: () => void): void;

  forceUpdate(callback?: () => void): void;

  // lifecycle methods

  render(): ?React$Element<any>;
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
declare class LegacyReactComponent<DefaultProps, Props, State> extends React$Component<DefaultProps, Props, State> {
  // additional methods

  getDefaultProps(): DefaultProps;
  getInitialState(): State;

  replaceState(state: State, callback?: () => void): void;

  isMounted(): bool;

  // TODO: Due to bugs in Flow's handling of React.createClass, some fields
  // already declared in the base class need to be redeclared below. Ideally
  // they should simply be inherited.

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
 * of React$Component. We make the type of a React class parametric over Config,
 * which is derived from some of the type parameters (DefaultProps, Props) of
 * React$Component, abstracting away others (State); whereas a React$Component
 * type is useful for checking the definition of a React class, a ReactClass
 * type (and the corresponding React$Element type, see below) is useful for
 * checking the uses of a React class. The required constraints are set up using
 * a "helper" type alias, that takes an additional type parameter C representing
 * the React class, which is then abstracted with an existential type (*). The *
 * can be thought of as an "auto" instruction to the typechecker, telling it to
 * fill in the type from context.
 */
type ReactClass<Config> = _ReactClass<*, *, Config, *>;
type _ReactClass<DefaultProps, Props, Config: $Diff<Props, DefaultProps>, C: React$Component<DefaultProps, Props, any>> = Class<C>;

// TODO: DefaultProps and Props are needed to type some introspection APIs
// below, but in general, Config does not carry enough information to recover
// them faithfully. For now, we trade off that additional precision for overall
// simplicity of the model. In the future, we may provide more powerful
// introspection features in types, at which point Config would carry enough
// information to faithfully recover DefaultProps and Props.
type $PropsOf<Config> = any
type $DefaultPropsOf<Config> = any

/**
 * Type of a React element. React elements are commonly created using JSX
 * literals, which desugar to React.createElement calls (see below). The type
 * parameterization of React$Element mimics that of ReactClass (see above).
 */
declare class React$Element<Config> {
  type: ReactClass<Config>;
  props: $PropsOf<Config>;
  key: ?string;
  ref: any;
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
  declare function createClass(spec: any): ReactClass<any>;

  declare function cloneElement<Config> (
    element: React$Element<Config>,
    attributes: $Shape<Config>,
    children?: any
  ): React$Element<Config>;

  /**
   * Methods that take an `config` argument of type Config, describing objects
   * whose properties must cover (at least) the difference between the
   * properties in Props and the properties in DefaultProps.  This is intended
   * to model what happens at run time: the config are merged with the default
   * props to obtain the props of a React$Element / React$Component instance.
   */
  declare var createElement: React$CreateElement

  // TODO: React DOM elements
  declare function createFactory<Config>(
    name: ReactClass<Config>
  ): (config: Config, children?: any) => React$Element<Config>;

  // TODO: React DOM elements
  declare function constructAndRenderComponent<Config>(
    name: ReactClass<Config>,
    config: Config,
    container: any
  ): React$Component<$DefaultPropsOf<Config>, $PropsOf<Config>, any>;

  // TODO: React DOM elements
  declare function constructAndRenderComponentByID<Config>(
    name: ReactClass<Config>,
    config: Config,
    id: string
  ): React$Component<$DefaultPropsOf<Config>, $PropsOf<Config>, any>;

  declare function findDOMNode(
    object: React$Component<any, any, any> | HTMLElement
  ): any;

  declare function render<Config>(
    element: React$Element<Config>,
    container: any
  ): React$Component<$DefaultPropsOf<Config>, $PropsOf<Config>, any>;

  declare function renderToString(
    element: React$Element<any>
  ): string;
  declare function renderToStaticMarkup(
    element: React$Element<any>
  ): string;

  declare function unmountComponentAtNode(container: any): boolean;

  declare function isValidElement(element: any): boolean;
  declare function withContext(context: any, callback: () => void): any;

  declare var Component: typeof React$Component;
  declare var Element: typeof React$Element;
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
    object: React$Component<any, any, any> | HTMLElement
  ): any;

  declare function render<Config>(
    element: React$Element<Config>,
    container: any
  ): React$Component<$DefaultPropsOf<Config>, $PropsOf<Config>, any>;

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
  declare function unstable_renderSubtreeIntoContainer<Config>(
    parentComponent: React$Component<any, any, any>,
    nextElement: React$Element<Config>,
    container: any,
    callback?: () => void
  ): React$Component<$DefaultPropsOf<Config>, $PropsOf<Config>, any>;
}

// TODO: Delete the corresponding method defs from the 'react' module once React
// 0.15 comes out and removes them.
declare module 'react-dom/server' {
  declare function renderToString(
    element: React$Element<any>
  ): string;
  declare function renderToStaticMarkup(
    element: React$Element<any>
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
    persist(): void;
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

type $JSXIntrinsics = {[k:string]: ReactClass<any>}
