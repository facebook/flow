/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */
/* React */

/**
 * Base class of all React classes, modeled as a polymorphic class whose type
 * parameters are DefaultProps, PropTypes, State.
 */
declare class ReactComponent<DefaultProps, PropTypes, State> {
  props: PropTypes;
  state: State;
  refs: any;
  context: any;

  getInitialState(): State;
  getChildContext(): any;

  setProps(props: $Shape<PropTypes>, callback?: () => void): void;
  replaceProps(props: PropTypes, callback?: () => void): void;

  setState(state: $Shape<State>, callback?: () => void): void;
  replaceState(state: State, callback?: () => void): void;

  render(): ?ReactElement<any, any, any>;

  forceUpdate(callback?: () => void): void;
  getDOMNode(): any;
  componentWillMount(): void;
  componentDidMount(component?: any): void;
  componentWillReceiveProps(nextProps: PropTypes, nextContext?: any): void;
  shouldComponentUpdate(nextProps: PropTypes, nextState: State, nextContext: any): boolean;
  componentWillUpdate(nextProps: PropTypes, nextState: State, nextContext: any): void;
  componentDidUpdate(nextProps: PropTypes, nextState: State, nextContext: any, component: any): void;
  componentWillUnmount(): void;
  isMounted(): bool;

  static propTypes: $Subtype<{[_: $Keys<PropTypes>]: any}>; //object whose keys are in PropTypes
  static contextTypes: any;
  static childContextTypes: any;
  static displayName: string;
  static defaultProps: DefaultProps;
}

/**
 * Type of a React class (not to be confused with the type of instances of a
 * React class, which is the React class itself). A React class is any subclass
 * of ReactComponent. We make the type of a React class polymorphic over the
 * same type parameters (DefaultProps, PropTypes, State) as ReactComponent. The required constraints
 * are set up using a "helper" type alias, that takes an additional type
 * parameter C representing the React class, which is then abstracted with an
 * existential type (*). The * can be thought of as an "auto" instruction to the
 * typechecker, telling it to fill in the type from context.
 */
type ReactClass<DefaultProps, PropTypes, State> = _ReactClass<DefaultProps, PropTypes, State, *>;
type _ReactClass<DefaultProps, PropTypes, State, C: ReactComponent<DefaultProps, PropTypes, State>> = Class<C>;

/**
 * Type of a React element. React elements are commonly created using JSX
 * literals, which desugar to React.createElement calls (see below).
 */
declare class ReactElement<DefaultProps, PropTypes, State> {
    type: ReactClass<DefaultProps, PropTypes, State>;
    props: PropTypes;
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

  declare function cloneElement<DefaultProps, PropTypes, State> (
    element: ReactElement<DefaultProps, PropTypes, State>,
    attributes: $Shape<PropTypes>,
    children?: any
  ): ReactElement<DefaultProps, PropTypes, State>;

  /**
   * Methods that take an `attributes` argument of type A (= Attributes),
   * describing objects whose properties must cover (at least) the difference
   * between the properties in PropTypes and the properties in DefaultProps.
   * This is intended to model what happens at run time: the
   * attributes are merged with the default props to obtain the props of a
   * ReactElement / ReactComponent instance.
   */
  // TODO: React DOM elements
  declare function createElement<DefaultProps, PropTypes, State, A: $Diff<PropTypes, DefaultProps>>(
    name: ReactClass<DefaultProps, PropTypes, State>,
    attributes: A,
    children?: any
  ): ReactElement<DefaultProps, PropTypes, State>;

  // TODO: React DOM elements
  declare function createFactory<DefaultProps, PropTypes, State, A: $Diff<PropTypes, DefaultProps>>(
    name: ReactClass<DefaultProps, PropTypes, State>
  ): (attributes: A, children?: any) => ReactElement<DefaultProps, PropTypes, State>;

  // TODO: React DOM elements
  declare function constructAndRenderComponent<DefaultProps, PropTypes, State, A: $Diff<PropTypes, DefaultProps>>(
    name: ReactClass<DefaultProps, PropTypes, State>,
    attributes: A,
    container: any
  ): ReactComponent<DefaultProps, PropTypes, State>;

  // TODO: React DOM elements
  declare function constructAndRenderComponentByID<DefaultProps, PropTypes, State, A: $Diff<PropTypes, DefaultProps>>(
    name: ReactClass<DefaultProps, PropTypes, State>,
    attributes: A,
    id: string
  ): ReactComponent<DefaultProps, PropTypes, State>;

  declare function findDOMNode(
    object: ReactComponent<any, any, any> | HTMLElement
  ): any;

  declare function render<DefaultProps, PropTypes, State>(
    element: ReactElement<DefaultProps, PropTypes, State>,
    container: any
  ): ReactComponent<DefaultProps, PropTypes, State>;

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

declare class SyntheticEvent {
    bubbles: boolean;
    cancelable: boolean;
    currentTarget: EventTarget;
    defaultPrevented: boolean;
    eventPhase: number;
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
