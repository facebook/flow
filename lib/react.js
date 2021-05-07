/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * A UI node that can be rendered by React. React can render most primitives in
 * addition to elements and arrays of nodes.
 */

declare type React$Node =
  | null
  | boolean
  | number
  | string
  | React$Element<any>
  | React$Portal
  | Iterable<?React$Node>;


export type React$TransportValue =
  | void
  | null
  | boolean
  | number
  | string
  | React$Element<React$ElementType>
  | $ReadOnlyArray<React$TransportValue>
  | React$TransportObject;

export type React$TransportObject = {
  +[string]: React$TransportValue,
};

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
    partialState: ?$Shape<State> | ((State, Props) => ?$Shape<State>),
    callback?: () => mixed,
  ): void;

  forceUpdate(callback?: () => void): void;

  // lifecycle methods

  constructor(props?: Props, context?: any): void;
  render(): React$Node;
  componentWillMount(): mixed;
  UNSAFE_componentWillMount(): mixed;
  componentDidMount(): mixed;
  componentWillReceiveProps(
    nextProps: Props,
    nextContext: any,
  ): mixed;
  UNSAFE_componentWillReceiveProps(
    nextProps: Props,
    nextContext: any,
  ): mixed;
  shouldComponentUpdate(
    nextProps: Props,
    nextState: State,
    nextContext: any,
  ): boolean;
  componentWillUpdate(
    nextProps: Props,
    nextState: State,
    nextContext: any,
  ): mixed;
  UNSAFE_componentWillUpdate(
    nextProps: Props,
    nextState: State,
    nextContext: any,
  ): mixed;
  componentDidUpdate(
    prevProps: Props,
    prevState: State,
    prevContext: any,
  ): mixed;
  componentWillUnmount(): mixed;
  componentDidCatch(
    error: Error,
    info: { componentStack: string, ... }
  ): mixed;

  // long tail of other stuff not modeled very well

  refs: any;
  context: any;
  getChildContext(): any;
  static displayName?: ?string;
  static childContextTypes: any;
  static contextTypes: any;
  static propTypes: any;

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

declare type React$AbstractComponentStatics = {
  displayName?: ?string,
  // This is only on function components, but trying to access name when
  // displayName is undefined is a common pattern.
  name?: ?string,
  propTypes?: {[string] : any, ...},
  ...
};

/**
 * The type of a stateless functional component. In most cases these components
 * are a single function. However, they may have some static properties that we
 * can type check.
 */
declare type React$StatelessFunctionalComponent<Props> = {
  (props: Props, context: any): React$Node,
  displayName?: ?string,
  propTypes?: any,
  contextTypes?: any,
  ...
};

/**
 * The type of a component in React. A React component may be a:
 *
 * - Stateless functional components. Functions that take in props as an
 *   argument and return a React node.
 * - ES6 class component. Components with state defined either using the ES6
 *   class syntax, or with the legacy `React.createClass()` helper.
 */
declare type React$ComponentType<-Config> = React$AbstractComponent<Config, mixed>;

/**
 * The type of an element in React. A React element may be a:
 *
 * - String. These elements are intrinsics that depend on the React renderer
 *   implementation.
 * - React component. See `ComponentType` for more information about its
 *   different variants.
 */
declare type React$ElementType =
  | string
  | React$AbstractComponent<empty, mixed>;

/**
 * Type of a React element. React elements are commonly created using JSX
 * literals, which desugar to React.createElement calls (see below).
 */
declare type React$Element<+ElementType: React$ElementType> = {|
  +type: ElementType,
  +props: React$ElementProps<ElementType>,
  +key: React$Key | null,
  +ref: any,
|};

declare type React$MixedElement = React$Element<React$ElementType>;

/**
 * The type of the key that React uses to determine where items in a new list
 * have moved.
 */
declare type React$Key = string | number;

/**
 * The type of the ref prop available on all React components.
 */
declare type React$Ref<ElementType: React$ElementType> =
  | { -current: React$ElementRef<ElementType> | null, ... }
  | ((React$ElementRef<ElementType> | null) => mixed)
  | number | string;

/**
 * The type of a React Context.  React Contexts are created by calling
 * createContext() with a default value.
 */
declare type React$Context<T> = {
  Provider: React$ComponentType<{
    value: T,
    children?: React$Node,
    ...
  }>,
  Consumer: React$ComponentType<{ children: (value: T) => ?React$Node, ... }>,
  // Optional, user-specified value for custom display label in React DevTools.
  displayName?: string,
  ...
}

/**
 * A React portal node. The implementation of the portal node is hidden to React
 * users so we use an opaque type.
 */
declare opaque type React$Portal;

declare type React$FragmentType = ({ children?: React$Node, ... }) => React$Node;

declare module react {
  declare export var DOM: any;
  declare export var PropTypes: ReactPropTypes;
  declare export var version: string;

  declare export function checkPropTypes<V>(
    propTypes : any,
    values: V,
    location: string,
    componentName: string,
    getStack: ?(() => ?string)
  ) : void;

  declare export var createClass: React$CreateClass;
  declare export function createContext<T>(
    defaultValue: T,
    calculateChangedBits: ?(a: T, b: T) => number,
  ): React$Context<T>;
  declare export var createElement: React$CreateElement;
  declare export var cloneElement: React$CloneElement;
  declare export function createFactory<ElementType: React$ElementType>(
    type: ElementType,
  ): React$ElementFactory<ElementType>;
  declare export function createRef<T>(
  ): {|current: null | T|};

  declare export function isValidElement(element: any): boolean;

  declare export var Component: typeof React$Component;
  declare export var PureComponent: typeof React$PureComponent;
  declare export type StatelessFunctionalComponent<P> =
    React$StatelessFunctionalComponent<P>;
  declare export type ComponentType<-P> = React$ComponentType<P>;
  declare export type AbstractComponent<
    -Config,
    +Instance = mixed,
  > = React$AbstractComponent<Config, Instance>;
  declare export type MixedElement = React$MixedElement;
  declare export type ElementType = React$ElementType;
  declare export type Element<+C> = React$Element<C>;
  declare export var Fragment: React$FragmentType;
  declare export type Key = React$Key;
  declare export type Ref<C> = React$Ref<C>;
  declare export type Node = React$Node;
  declare export type TransportObject = React$TransportObject;
  declare export type TransportValue = React$TransportValue;
  declare export type Context<T> = React$Context<T>;
  declare export type Portal = React$Portal;
  declare export var ConcurrentMode: ({ children?: React$Node, ... }) => React$Node; // 16.7+
  declare export var StrictMode: ({ children?: React$Node, ... }) => React$Node;

  declare export var Suspense: React$ComponentType<{
    children?: React$Node,
    fallback?: React$Node,
    ...
  }>; // 16.6+

  declare export type ElementProps<C> = React$ElementProps<C>;
  declare export type ElementConfig<C> = React$ElementConfig<C>;
  declare export type ElementRef<C> = React$ElementRef<C>;
  declare export type Config<Props, DefaultProps> = React$Config<Props, DefaultProps>;

  declare export type ChildrenArray<+T> = $ReadOnlyArray<ChildrenArray<T>> | T;
  declare export var Children: {
    map<T, U>(
      children: ChildrenArray<T>,
      fn: (child: $NonMaybeType<T>, index: number) => U,
      thisArg?: mixed,
    ): Array<$NonMaybeType<U>>,
    forEach<T>(
      children: ChildrenArray<T>,
      fn: (child: T, index: number) => mixed,
      thisArg?: mixed,
    ): void,
    count(children: ChildrenArray<any>): number,
    only<T>(children: ChildrenArray<T>): $NonMaybeType<T>,
    toArray<T>(children: ChildrenArray<T>): Array<$NonMaybeType<T>>,
    ...
  };

  declare export function forwardRef<Config, Instance>(
    render: (
      props: Config,
      ref: { current: null | Instance, ... } | ((null | Instance) => mixed),
    ) => React$Node,
  ): React$AbstractComponent<Config, Instance>;

  declare export function memo<Config, Instance = mixed>(
    component: React$AbstractComponent<Config, Instance>,
    equal?: (Config, Config) => boolean,
  ): React$AbstractComponent<Config, Instance>;

  declare export function lazy<Config, Instance = mixed>(
    component: () => Promise<{ default: React$AbstractComponent<Config, Instance>, ... }>,
  ): React$AbstractComponent<Config, Instance>;

  declare type MaybeCleanUpFn = void | (() => void);

  declare export function useContext<T>(context: React$Context<T>): T;
    
  declare type NonFunctions = number | boolean | Object | string | null | void | Array<NonFunctions>;
  declare var useState:
    & (<S: NonFunctions>(initialState: (() => S) | S) => [S, ((S => S) | S) => void])
    & (<S: Function>(initialState: (() => S)) => [S, ((S => S)) => void]);

  declare type Dispatch<A> = (A) => void;

  declare export function useReducer<S, A>(
    reducer: (S, A) => S,
    initialState: S,
  ): [S, Dispatch<A>];

  declare export function useReducer<S, A>(
    reducer: (S, A) => S,
    initialState: S,
    init: void,
  ): [S, Dispatch<A>];

  declare export function useReducer<S, A, I>(
    reducer: (S, A) => S,
    initialArg: I,
    init: (I) => S,
  ): [S, Dispatch<A>];

  declare export function useRef<T>(initialValue: T): {|current: T|};

  declare export function useDebugValue(value: any): void;

  declare export function useEffect(
    create: () => MaybeCleanUpFn,
    inputs?: ?$ReadOnlyArray<mixed>,
  ): void;

  declare export function useLayoutEffect(
    create: () => MaybeCleanUpFn,
    inputs?: ?$ReadOnlyArray<mixed>,
  ): void;

  declare export function useCallback<T: (...args: $ReadOnlyArray<empty>) => mixed>(
    callback: T,
    inputs: ?$ReadOnlyArray<mixed>,
  ): T;

  declare export function useMemo<T>(
    create: () => T,
    inputs: ?$ReadOnlyArray<mixed>,
  ): T;

  declare export function useImperativeHandle<T>(
    ref: { current: T | null, ... } | ((inst: T | null) => mixed) | null | void,
    create: () => T,
    inputs: ?$ReadOnlyArray<mixed>,
  ): void;

  declare export function useDeferredValue<T>(value: T): T;

  declare export function useTransition(): [boolean, (() => void) => void];

  declare export function startTransition(() => void): void;


  declare export type Interaction = {
    name: string,
    timestamp: number,
    ...
  };

  declare type ProfilerOnRenderFnType = (
    id: string,
    phase: "mount" | "update",
    actualDuration: number,
    baseDuration: number,
    startTime: number,
    commitTime: number,
    interactions: Set<Interaction>,
  ) => void;

  declare export var Profiler: React$AbstractComponent<{|
    children?: React$Node,
    id: string,
    onRender: ProfilerOnRenderFnType,
  |}, void>;

  declare type TimeoutConfig = {|
    timeoutMs: number,
  |};

  declare export default {|
    +DOM: typeof DOM,
    +PropTypes: typeof PropTypes,
    +version: typeof version,
    +checkPropTypes: typeof checkPropTypes,
    +memo: typeof memo,
    +lazy: typeof lazy,
    +createClass: typeof createClass,
    +createContext: typeof createContext,
    +createElement: typeof createElement,
    +cloneElement: typeof cloneElement,
    +createFactory: typeof createFactory,
    +createRef: typeof createRef,
    +forwardRef: typeof forwardRef,
    +isValidElement: typeof isValidElement,
    +Component: typeof Component,
    +PureComponent: typeof PureComponent,
    +Fragment: React$FragmentType,
    +Children: typeof Children,
    +ConcurrentMode: typeof ConcurrentMode,
    +StrictMode: typeof StrictMode,
    +Profiler: typeof Profiler,
    +Suspense: typeof Suspense,
    +useContext: typeof useContext,
    +useState: typeof useState,
    +useReducer: typeof useReducer,
    +useRef: typeof useRef,
    +useEffect: typeof useEffect,
    +useLayoutEffect: typeof useLayoutEffect,
    +useCallback: typeof useCallback,
    +useMemo: typeof useMemo,
    +useImperativeHandle: typeof useImperativeHandle,
    +useTransition: typeof useTransition,
    +useDeferredValue: typeof useDeferredValue,
    +startTransition: typeof startTransition,
  |};
}

// TODO Delete this once https://github.com/facebook/react/pull/3031 lands
// and "react" becomes the standard name for this module
declare module React {
  declare module.exports: $Exports<'react'>;
}

type ReactPropsCheckType = (
  props: any,
  propName: string,
  componentName: string,
  href?: string) => ?Error;

type ReactPropsChainableTypeChecker = {
  (props: any, propName: string, componentName: string, href?: string): ?Error,
  isRequired: ReactPropsCheckType,
  ...
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
  (shapeTypes: { [key: string]: ReactPropsCheckType, ... }) =>
    ReactPropsChainableTypeChecker;

type ReactPropTypes = {
  array: React$PropType$Primitive<Array<any>>,
  bool: React$PropType$Primitive<boolean>,
  func: React$PropType$Primitive<Function>,
  number: React$PropType$Primitive<number>,
  object: React$PropType$Primitive<Object>,
  string: React$PropType$Primitive<string>,
  any: React$PropType$Primitive<any>,
  arrayOf: React$PropType$ArrayOf,
  element: React$PropType$Primitive<any>,
  /* TODO */
  instanceOf: React$PropType$InstanceOf,
  node: React$PropType$Primitive<any>,
  /* TODO */
  objectOf: React$PropType$ObjectOf,
  oneOf: React$PropType$OneOf,
  oneOfType: React$PropType$OneOfType,
  shape: React$PropType$Shape,
  ...
}

declare module '#flow-internal-react-server-module' {
  declare export var createElement: React$CreateElement;
  declare export type Node = React$Node;
  declare export default {|
    +createElement: typeof createElement,
  |};
}
