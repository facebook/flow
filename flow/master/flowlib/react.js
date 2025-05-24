/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

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
  | React$Portal
  | Iterable<?React$Node>;

declare type React$AbstractComponentStatics = {
  displayName?: ?string,
  // This is only on function components, but trying to access name when
  // displayName is undefined is a common pattern.
  name?: ?string,
  propTypes?: {[string] : any, ...},
  ...
};

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
  | component(...empty);

/**
 * Type of a React element. React elements are commonly created using JSX
 * literals, which desugar to React.createElement calls (see below).
 *
 * The internal structure of React.Element is intentionally omitted to
 * discourage React.Element inspection.
 *
 * See https://react.dev/reference/react/Children#alternatives for alternatives.
 *
 * @deprecated You should not require very specific React elements.
 * Use React.Node, React.MixedElement, or [render types](https://flow.org/en/docs/react/render-types/) instead.
 */
declare opaque type React$Element<+ElementType: React$ElementType, +P = React$ElementProps<ElementType>>: {...};

/**
 * A type that should be used as the arguments of a render type.
 * Referencing nominal components in type positions will produce this type.
 *
 * e.g.
 * ```flow
 * declare component Foo();
 *
 * type T1 = renders Foo;
 * // Desugars to
 * type T1 = renders React$RendersExactly<typeof Foo>;
 * ```
 */
declare opaque type React$RendersExactly<+ElementType: React$ElementType>: React$Node;

/**
 * Type of a React element. React elements are commonly created using JSX
 * literals, which desugar to React.createElement calls (see below).
 *
 * The internal structure of React.Element is intentionally omitted to
 * discourage React.Element inspection.
 *
 * See https://react.dev/reference/react/Children#alternatives for alternatives.
 *
 * @deprecated You should not require very specific React elements.
 * Use React.Node, React.MixedElement, or [render types](https://flow.org/en/docs/react/render-types/) instead.
 */
declare type ExactReactElement_DEPRECATED<+C, +P = React$ElementProps<C>> = React$Element<C, P>;

declare type React$MixedElement = React$Element<React$ElementType>;

/**
 * The type of the key that React uses to determine where items in a new list
 * have moved.
 */
declare type React$Key = string | number;

declare opaque type React$RefObject<T>: {| current: T |}

/**
 * The type of the `ref` prop on forwardRef components and the ref argument
 * for useImperativeHandle.
 */
declare type React$RefSetter<-T> =
  | { -current: T | null, ... }
  | ((T | null) => mixed)
  | null
  | void;


/**
 * The type of a React Context.  React Contexts are created by calling
 * createContext() with a default value.
 */
declare opaque type React$Context<T>:
component<Renders: React$Node = React$Node>(value: T, children?: Renders, ...{...}) renders Renders
& {
  Provider: component<Renders: React$Node = React$Node>(value: T, children?: Renders, ...{...}) renders Renders,
  Consumer: React.ComponentType<{ +children: (value: T) => ?React$Node, ... }>,
  // Optional, user-specified value for custom display label in React DevTools.
  displayName?: string,
  ...
}

/**
 * A React portal node. The implementation of the portal node is hidden to React
 * users so we use an opaque type.
 */
declare opaque type React$Portal;

declare namespace React {
  type ComponentType<-P: {...}> = component(...P);
  type PropsOf<E: string | React$MixedElement | React$RendersExactly<React$ElementType>> = E extends React$Element<infer C> ? React$ElementConfig<C> : E extends React$RendersExactly<infer C> ? React$ElementConfig<C> : (E extends string ? $JSXIntrinsics[E]['props'] : empty);
  type PropOf<E: string | React$MixedElement | React$RendersExactly<React$ElementType>, K: string> = PropsOf<E>[K]
  type RefOf<E: string | React$MixedElement | React$RendersExactly<React$ElementType>> = E extends React$Element<infer C> ? React$ElementRef<C> : E extends React$RendersExactly<infer C> ? React$ElementRef<C> : (E extends string ? $JSXIntrinsics[E]['instance'] : empty);
  type MixedElement = React$MixedElement;
  type ElementType = React$ElementType;

  type Key = React$Key;
  type RefSetter<-T> = React$RefSetter<T>;
  type Node = React$Node;
  type Context<T> = React$Context<T>;
  type Portal = React$Portal;

  type ElementProps<C> = React$ElementProps<C>;
  type ElementConfig<C> = React$ElementConfig<C>;
  type ElementRef<C> = React$ElementRef<C>;

  type ChildrenArray<+T> = $ReadOnlyArray<ChildrenArray<T>> | T;
  type ReactSetStateFunction<S> = ((S => S) | S) => void;
  type RefObject<T> = React$RefObject<T>;
  type Interaction = {
    name: string,
    timestamp: number,
    ...
  };
}

/**
 * Intentionally fully opaque to ban direct calls to `React.createElement`.
 * Use JSX instead.
 * @see https://react.dev/blog/2024/04/25/react-19-upgrade-guide#new-jsx-transform-is-now-required
 */
declare opaque type React$CreateElement;

type React$CloneElement =
  & (<C: $Keys<$JSXIntrinsics>, E: React$Element<C>>(
    element: E,
    props?: ?Partial<$JSXIntrinsics[C]['props']>,
    ...children: $ReadOnlyArray<React$Node>
  ) => E)
  & (<Props, C: component(...empty), E: React$Element<C, Props>>(
    element: E,
    props?: ?Partial<$ReadOnly<{|...$Exact<NoInfer<Props>>, key: React$Key, ref: React$RefSetter<React$ElementRef<NoInfer<C>>>|}>>,
  ) => E)
  & (<Props, C: component(...empty), E: React$Element<C, Props>>(
    element: E,
    props: ?Partial<$ReadOnly<{|...$Exact<NoInfer<Props>>, key: React$Key, ref: React$RefSetter<React$ElementRef<NoInfer<C>>>|}>>,
    children: NoInfer<Props>['children']
  ) => E)
  & (<Props, C: component(...empty), E: React$Element<C, Props>>(
    element: E,
    props: ?Partial<$ReadOnly<{|...$Exact<NoInfer<Props>>, key: React$Key, ref: React$RefSetter<React$ElementRef<NoInfer<C>>>|}>>,
    firstChild: NoInfer<Props>['children'][0],
    secondChild: NoInfer<Props>['children'][1],
    ...restChildren: NoInfer<Props>['children'] extends [+first: mixed, +second: mixed, ...infer Rest] ? Rest : NoInfer<Props>['children']
  ) => E);

declare module react {
  declare export const version: string;
  /**
   * `createContext` lets you create a context that components can provide or read.
   *
   * @see https://react.dev/reference/react/createContext
   */
  declare export const createContext: typeof React.createContext;
  /**
   * `createElement` lets you create a React element.
   * It serves as an alternative to writing JSX.
   *
   * @see https://react.dev/reference/react/createElement
   */
  declare export const createElement: React$CreateElement;
  /**
   * Using cloneElement is uncommon and can lead to fragile code.
   * [See common alternatives](https://react.dev/reference/react/cloneElement#alternatives)
   *
   * @see https://react.dev/reference/react/cloneElement
   */
  declare export const cloneElement: React$CloneElement;
  /**
   * `createRef` creates a ref object which can contain arbitrary value.
   *
   * `createRef` is mostly used for class components.
   * Function components typically rely on `useRef` instead.
   *
   * @see https://react.dev/reference/react/createRef
   */
  declare export const createRef: typeof React.createRef;
  /**
   * `isValidElement` checks whether a value is a React element.
   *
   * It is very uncommon to need isValidElement.
   * It’s mostly useful if you’re calling another API that only accepts
   * elements (like cloneElement does) and you want to avoid an error
   * when your argument is not a React element.
   *
   * Unless you have some very specific reason to add an isValidElement check,
   * you probably don’t need it.
   *
   * @see https://react.dev/reference/react/isValidElement
   */
  declare export function isValidElement(element: any): boolean;

  /**
   * `Component` lets you define a React component as a JavaScript class.
   *
   * We recommend defining components as functions instead of classes.
   * [See how to migrate](https://react.dev/reference/react/PureComponent#alternatives).
   *
   * @see https://react.dev/reference/react/Component
   */
  declare export const Component: typeof React.Component;

  /**
   * `PureComponent` is similar to `Component`, but it skip re-renders with same props.
   *
   * We recommend defining components as functions instead of classes.
   * [See how to migrate](https://react.dev/reference/react/PureComponent#alternatives).
   *
   * @see https://react.dev/reference/react/PureComponent
   */
  declare export const PureComponent: typeof React.PureComponent;
  declare export type ComponentType<-P: {...}> = component(...P);
  declare export type PropsOf<C: string | React$MixedElement | React$RendersExactly<React$ElementType>> = React.PropsOf<C>;
  declare export type PropOf<C: string | React$MixedElement | React$RendersExactly<React$ElementType>, K: string> = React.PropOf<C, K>;
  declare export type RefOf<C: string | React$MixedElement | React$RendersExactly<React$ElementType>> = React.RefOf<C>;
  declare export type MixedElement = React$MixedElement;
  declare export type ElementType = React$ElementType;

  /**
   * `<Fragment>`, often used via `<>...</>` syntax,
   * lets you group elements without a wrapper node.
   *
   * @see https://react.dev/reference/react/Fragment
   */
  declare export const Fragment: typeof React.Fragment;

  declare export type Key = React$Key;
  declare export type RefSetter<-T> = React$RefSetter<T>;
  declare export type Node = React$Node;
  declare export type Context<T> = React$Context<T>;
  declare export type Portal = React$Portal;
  /**
   * `<StrictMode>` lets you find common bugs in your components early
   * during development.
   *
   * @see https://react.dev/reference/react/StrictMode
   */
  declare export const StrictMode: typeof React.StrictMode;

  /**
   * `<Suspense>` lets you display a fallback until its children have finished
   * loading.
   *
   * @see https://react.dev/reference/react/Suspense
   */
  declare export const Suspense: typeof React.Suspense; // 16.6+

  declare export type ElementProps<C> = React$ElementProps<C>;
  declare export type ElementConfig<C> = React$ElementConfig<C>;
  declare export type ElementRef<C> = React$ElementRef<C>;

  declare interface Thenable<T> {
    then(
      onFulfill: (value: T) => any,
      onReject: (error: mixed) => mixed,
    ): mixed;
  }

  declare type Usable<T> = Thenable<T> | React$Context<T>;

  declare export type ChildrenArray<+T> = React.ChildrenArray<T>;
  /**
   * `Children`lets you manipulate and transform the JSX received as the
   * children prop.
   *
   * Using `Children` is uncommon and can lead to fragile code.
   * [See common alternatives](https://react.dev/reference/react/Children#alternatives).
   *
   * @see https://react.dev/reference/react/Children
   */
  declare export const Children: typeof React.Children;

  /**
   * In React 19, `forwardRef` is no longer necessary. Pass `ref` as a prop instead.
   * forwardRef will deprecated in a future release.
   *
   * @see https://react.dev/blog/2024/12/05/react-19#ref-as-a-prop
   * @deprecated
   */
  declare export const forwardRef: typeof React.forwardRef;

  /**
   * `memo` lets you skip re-rendering a component when its props are unchanged.
   *
   * @see https://react.dev/reference/react/memo
   */
  declare export const memo: typeof React.memo;

  /**
   * `lazy` lets you defer loading component’s code until it is rendered for
   * the first time.
   *
   * @see https://react.dev/reference/react/lazy
   */
  declare export const lazy: typeof React.lazy;

  declare type MaybeCleanUpFn = void | (() => void);
  declare export type ReactSetStateFunction<S> = React.ReactSetStateFunction<S>;

  /**
   * `use` is a React Hook that lets you read the value of a resource like a
   * `Promise` or `context`.
   *
   * @see https://react.dev/reference/react/use
   */
  declare export function use<T>(usable: Usable<T>): T;

  /**
   * `useContext` is a React Hook that lets you read and subscribe to context
   * from your component.
   *
   * @see https://react.dev/reference/react/useContext
   */
  declare export const useContext: typeof React.useContext;

  /**
   * `useState` is a React Hook that lets you add a
   * [state variable](https://react.dev/learn/state-a-components-memory)
   * to your component.
   *
   * @see https://react.dev/reference/react/useState
   */
  declare export const useState: typeof React.useState;

  declare type Dispatch<A> = (A) => void;

  /**
   * `useReducer` is a React Hook that lets you add a
   * [reducer](https://react.dev/learn/extracting-state-logic-into-a-reducer)
   * to your component.
   *
   * @see https://react.dev/reference/react/useReducer
   */
  declare export const useReducer: typeof React.useReducer;

  declare export type RefObject<T> = React$RefObject<T>;
  /**
   * `useRef` is a React Hook that lets you reference a value that's
   * not needed for rendering.
   *
   * @see https://react.dev/reference/react/useRef
   */
  declare export const useRef: typeof React.useRef;

  /**
   * `useDebugValue` is a React Hook that lets you add a label to a custom Hook
   * in React DevTools.
   *
   * @see https://react.dev/reference/react/useDebugValue
   */
  declare export function useDebugValue(value: any): void;

  /**
   * `useEffect` is a React Hook that lets you
   * [synchronize a component with an external system](https://react.dev/learn/synchronizing-with-effects).
   *
   * @see https://react.dev/reference/react/useEffect
   */
  declare export const useEffect: typeof React.useEffect;

  /**
   * `useLayoutEffect` is a version of useEffect that fires before the browser
   * repaints the screen.
   *
   * `useLayoutEffect` can hurt performance. Prefer `useEffect` when possible.
   *
   * @see https://react.dev/reference/react/useLayoutEffect
   */
  declare export const useLayoutEffect: typeof React.useLayoutEffect;

  /**
   * `useCallback` is a React Hook that lets you cache a function definition
   * between re-renders.
   *
   * @see https://react.dev/reference/react/useCallback
   */
  declare export const useCallback: typeof React.useCallback;

  /**
   * useMemo is a React Hook that lets you cache the result of a calculation
   * between re-renders.
   *
   * @see https://react.dev/reference/react/useMemo
   */
  declare export const useMemo: typeof React.useMemo;

  /**
   * `useImperativeHandle` is a React Hook that lets you customize the handle
   * exposed as a ref.
   *
   * @see https://react.dev/reference/react/useImperativeHandle
   */
  declare export const useImperativeHandle: typeof React.useImperativeHandle;

  /**
   * `useDeferredValue` is a React Hook that lets you defer updating
   * a part of the UI.
   *
   * @see https://react.dev/reference/react/useDeferredValue
   */
  declare export const useDeferredValue: typeof React.useDeferredValue;

  /**
   * `useTransition` is a React Hook that lets you update the state
   * without blocking the UI.
   *
   * @see https://react.dev/reference/react/useTransition
   */
  declare export const useTransition: typeof React.useTransition;

  /**
   * `startTransition` lets you update the state without blocking the UI.
   *
   * @see https://react.dev/reference/react/startTransition
   */
  declare export const startTransition: typeof React.startTransition;

  /**
   * `useId` is a React Hook for generating unique IDs that
   * can be passed to accessibility attributes.
   *
   * @see https://react.dev/reference/react/useId
   */
  declare export const useId: typeof React.useId;

  /**
   * `useInsertionEffect` allows inserting elements into the DOM
   * before any layout effects fire.
   *
   * `useInsertionEffect` is for CSS-in-JS library authors. Unless you are
   * working on a CSS-in-JS library and need a place to inject the styles,
   * you probably want `useEffect` or `useLayoutEffect` instead.
   *
   * @see https://react.dev/reference/react/useInsertionEffect
   */
  declare export const useInsertionEffect: typeof React.useInsertionEffect;

  /**
   * `useSyncExternalStore` is a React Hook that lets you subscribe
   * to an external store.
   *
   * @see https://react.dev/reference/react/useSyncExternalStore
   */
  declare export const useSyncExternalStore: typeof React.useSyncExternalStore;

  /**
   * `useOptimistic` is a React Hook that lets you optimistically update
   * the UI.
   *
   * @see https://react.dev/reference/react/useOptimistic
   */
  declare export const useOptimistic: typeof React.useOptimistic;

  /**
   * `useActionState` is a Hook that allows you to update state based on the
   * result of a form action.
   *
   * @see https://react.dev/reference/react/useActionState
   */
  declare export const useActionState: typeof React.useActionState;

  declare export type Interaction = React.Interaction;

  declare type ProfilerOnRenderFnType = (
    id: string,
    phase: "mount" | "update",
    actualDuration: number,
    baseDuration: number,
    startTime: number,
    commitTime: number,
    interactions: Set<Interaction>,
  ) => void;

  /**
   * `<Profiler>` lets you measure rendering performance of a
   * React tree programmatically.
   *
   * @see https://react.dev/reference/react/Profiler
   */
  declare export const Profiler: typeof React.Profiler;

  declare type TimeoutConfig = {|
    timeoutMs: number,
  |};

  declare namespace React {
    declare const version: string;

    /**
     * `memo` lets you skip re-rendering a component when its props are unchanged.
     *
     * @see https://react.dev/reference/react/memo
     */
    declare function memo<Config: {...}, Ref, Renders: React.Node = React.Node>(
      component: component(ref?: Ref, ...Config) renders Renders,
      equal?: (Config, Config) => boolean,
    ): component(ref?: Ref, ...Config) renders Renders;

    /**
     * `lazy` lets you defer loading component’s code until it is rendered for
     * the first time.
     *
     * @see https://react.dev/reference/react/lazy
     */
    declare function lazy<Config: {...}, Ref, Renders: React.Node = React.Node>(
      component: () => Promise<
        $ReadOnly<{
          default: component(ref?: Ref, ...Config) renders Renders,
          ...
        }>,
      >,
    ): component(ref?: Ref, ...Config) renders Renders;

    /**
     * `createContext` lets you create a context that components can provide or read.
     *
     * @see https://react.dev/reference/react/createContext
     */
    declare function createContext<T>(
      defaultValue: T,
      calculateChangedBits: ?(a: T, b: T) => number,
    ): React$Context<T>;
    /**
     * `createElement` lets you create a React element.
     * It serves as an alternative to writing JSX.
     *
     * @see https://react.dev/reference/react/createElement
     */
    declare const createElement: React$CreateElement;

    /**
     * Using cloneElement is uncommon and can lead to fragile code.
     * [See common alternatives](https://react.dev/reference/react/cloneElement#alternatives)
     *
     * @see https://react.dev/reference/react/cloneElement
     */
    declare const cloneElement: React$CloneElement;

    /**
     * `createRef` creates a ref object which can contain arbitrary value.
     *
     * `createRef` is mostly used for class components.
     * Function components typically rely on `useRef` instead.
     *
     * @see https://react.dev/reference/react/createRef
     */
    declare function createRef<T>(
    ): React$RefObject<T | null>;
    /**
     * `isValidElement` checks whether a value is a React element.
     *
     * It is very uncommon to need isValidElement.
     * It’s mostly useful if you’re calling another API that only accepts
     * elements (like cloneElement does) and you want to avoid an error
     * when your argument is not a React element.
     *
     * Unless you have some very specific reason to add an isValidElement check,
     * you probably don’t need it.
     *
     * @see https://react.dev/reference/react/isValidElement
     */
    declare function isValidElement(element: any): boolean;

    /**
     * `Component` lets you define a React component as a JavaScript class.
     *
     * We recommend defining components as functions instead of classes.
     * [See how to migrate](https://react.dev/reference/react/PureComponent#alternatives).
     *
     * @see https://react.dev/reference/react/Component
     */
    declare class Component<Props, State = void> {
      // fields

      props: Props;
      state: State;

      // action methods

      setState(
        partialState: ?$ReadOnly<Partial<State>> | ((State, Props) => ?$ReadOnly<Partial<State>>),
        callback?: () => mixed,
      ): void;

      forceUpdate(callback?: () => void): void;

      // lifecycle methods

      constructor(props: Props, context?: any): void;
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
      static childContextTypes: empty;
      static contextTypes: empty;
      static propTypes: any;

      // We don't add a type for `defaultProps` so that its type may be entirely
      // inferred when we diff the type for `defaultProps` with `Props`. Otherwise
      // the user would need to define a type (which would be redundant) to override
      // the type we provide here in the base class.
      //
      // static defaultProps: $Shape<Props>;
    }

    /**
     * `PureComponent` is similar to `Component`, but it skip re-renders with same props.
     *
     * We recommend defining components as functions instead of classes.
     * [See how to migrate](https://react.dev/reference/react/PureComponent#alternatives).
     *
     * @see https://react.dev/reference/react/PureComponent
     */
    declare export class PureComponent<Props, State = void> extends Component<Props, State> {}

    /**
     * In React 19, `forwardRef` is no longer necessary. Pass `ref` as a prop instead.
     * forwardRef will deprecated in a future release.
     *
     * @see https://react.dev/blog/2024/12/05/react-19#ref-as-a-prop
     * @deprecated
     */
    declare function forwardRef<
      Config: {...},
      Instance,
      Renders: React$Node = React$Node,
    >(
      render: (props: Config, ref: React$RefSetter<Instance>) => Renders,
    ): component(ref: React.RefSetter<Instance>, ...Config) renders Renders;

    /**
     * `<Fragment>`, often used via `<>...</>` syntax,
     * lets you group elements without a wrapper node.
     *
     * @see https://react.dev/reference/react/Fragment
     */
    declare component Fragment<Renders: React$Node = void>(
      children?: Renders,
    ) renders Renders;

    /**
     * `Children`lets you manipulate and transform the JSX received as the
     * children prop.
     *
     * Using `Children` is uncommon and can lead to fragile code.
     * [See common alternatives](https://react.dev/reference/react/Children#alternatives).
     *
     * @see https://react.dev/reference/react/Children
     */
    declare const Children: {
      map<T, U, This>(
        children: ChildrenArray<T>,
        fn: (this : This, child: $NonMaybeType<T>, index: number) => U,
        thisArg: This,
      ): Array<$NonMaybeType<U>>,
      forEach<T, This>(
        children: ChildrenArray<T>,
        fn: (this : This, child: T, index: number) => mixed,
        thisArg: This,
      ): void,
      count(children: ChildrenArray<any>): number,
      only<T>(children: ChildrenArray<T>): $NonMaybeType<T>,
      toArray<T>(children: ChildrenArray<T>): Array<$NonMaybeType<T>>,
      ...
    };

    /**
     * `<StrictMode>` lets you find common bugs in your components early
     * during development.
     *
     * @see https://react.dev/reference/react/StrictMode
     */
    declare const StrictMode: ({ +children?: React$Node, ... }) => React$Node;

    /**
     * `<Suspense>` lets you display a fallback until its children have finished
     * loading.
     *
     * @see https://react.dev/reference/react/Suspense
     */
    declare const Suspense: React.ComponentType<{
      +children?: React$Node,
      +fallback?: React$Node,
      ...
    }>; // 16.6+

    /**
     * `<Profiler>` lets you measure rendering performance of a
     * React tree programmatically.
     *
     * @see https://react.dev/reference/react/Profiler
     */
    declare const Profiler: component(
      children?: React$Node,
      id: string,
      onRender: ProfilerOnRenderFnType,
    );

    /**
     * `useContext` is a React Hook that lets you read and subscribe to context
     * from your component.
     *
     * @see https://react.dev/reference/react/useContext
     */
    declare hook useContext<T>(context: React$Context<T>): T;

    /**
     * `useState` is a React Hook that lets you add a
     * [state variable](https://react.dev/learn/state-a-components-memory)
     * to your component.
     *
     * @see https://react.dev/reference/react/useState
     */
    declare hook useState<S>(
      initialState: (() => S) | S,
    ): [S, ReactSetStateFunction<S>];

    /**
     * `useReducer` is a React Hook that lets you add a
     * [reducer](https://react.dev/learn/extracting-state-logic-into-a-reducer)
     * to your component.
     *
     * @see https://react.dev/reference/react/useReducer
     */
    declare hook useReducer<S, A>(
      reducer: (S, A) => S,
      initialState: S,
    ): [S, Dispatch<A>];

    declare hook useReducer<S, A>(
      reducer: (S, A) => S,
      initialState: S,
      init: void,
    ): [S, Dispatch<A>];

    declare hook useReducer<S, A, I>(
      reducer: (S, A) => S,
      initialArg: I,
      init: (I) => S,
    ): [S, Dispatch<A>];

    /**
     * `useRef` is a React Hook that lets you reference a value that's
     * not needed for rendering.
     *
     * @see https://react.dev/reference/react/useRef
     */
    declare hook useRef<T>(initialValue: T): RefObject<T>;

    /**
     * `useEffect` is a React Hook that lets you
     * [synchronize a component with an external system](https://react.dev/learn/synchronizing-with-effects).
     *
     * @see https://react.dev/reference/react/useEffect
     */
    declare hook useEffect(
      create: () => MaybeCleanUpFn,
      inputs?: ?$ReadOnlyArray<mixed>,
    ): void;

    /**
     * `useLayoutEffect` is a version of useEffect that fires before the browser
     * repaints the screen.
     *
     * `useLayoutEffect` can hurt performance. Prefer `useEffect` when possible.
     *
     * @see https://react.dev/reference/react/useLayoutEffect
     */
    declare hook useLayoutEffect(
      create: () => MaybeCleanUpFn,
      inputs?: ?$ReadOnlyArray<mixed>,
    ): void;

    /**
     * `useCallback` is a React Hook that lets you cache a function definition
     * between re-renders.
     *
     * @see https://react.dev/reference/react/useCallback
     */
    declare hook useCallback<T: (...args: $ReadOnlyArray<empty>) => mixed>(
      callback: T,
      inputs: ?$ReadOnlyArray<mixed>,
    ): T;

    /**
     * useMemo is a React Hook that lets you cache the result of a calculation
     * between re-renders.
     *
     * @see https://react.dev/reference/react/useMemo
     */
    declare hook useMemo<T>(
      create: () => T,
      inputs: ?$ReadOnlyArray<mixed>,
    ): T;

    /**
     * `useImperativeHandle` is a React Hook that lets you customize the handle
     * exposed as a ref.
     *
     * @see https://react.dev/reference/react/useImperativeHandle
     */
    declare hook useImperativeHandle<T>(
      ref: React$RefSetter<T> | null | void,
      create: () => T,
      inputs: ?$ReadOnlyArray<mixed>,
    ): void;

    /**
     * `useDeferredValue` is a React Hook that lets you defer updating
     * a part of the UI.
     *
     * @see https://react.dev/reference/react/useDeferredValue
     */
    declare hook useDeferredValue<T>(value: T): T;

    /**
     * `useTransition` is a React Hook that lets you update the state
     * without blocking the UI.
     *
     * @see https://react.dev/reference/react/useTransition
     */
    declare hook useTransition(): [boolean, (() => void) => void];

    /**
     * `startTransition` lets you update the state without blocking the UI.
     *
     * @see https://react.dev/reference/react/startTransition
     */
    declare function startTransition(() => void): void;

    /**
     * `useId` is a React Hook for generating unique IDs that
     * can be passed to accessibility attributes.
     *
     * @see https://react.dev/reference/react/useId
     */
    declare hook useId(): string;

    /**
     * `useInsertionEffect` allows inserting elements into the DOM
     * before any layout effects fire.
     *
     * `useInsertionEffect` is for CSS-in-JS library authors. Unless you are
     * working on a CSS-in-JS library and need a place to inject the styles,
     * you probably want `useEffect` or `useLayoutEffect` instead.
     *
     * @see https://react.dev/reference/react/useInsertionEffect
     */
    declare hook useInsertionEffect(
      create: () => MaybeCleanUpFn,
      inputs?: ?$ReadOnlyArray<mixed>,
    ): void;

    /**
     * `useSyncExternalStore` is a React Hook that lets you subscribe
     * to an external store.
     *
     * @see https://react.dev/reference/react/useSyncExternalStore
     */
    declare hook useSyncExternalStore<Snapshot>(
      subscribe: (onStoreChange: () => void) => () => void,
      getSnapshot: () => Snapshot,
      getServerSnapshot?: () => Snapshot,
    ): Snapshot;

    /**
     * `useOptimistic` is a React Hook that lets you optimistically update
     * the UI.
     *
     * @see https://react.dev/reference/react/useOptimistic
     */
    declare hook useOptimistic<State>(
      passthrough: State,
    ): [State, (action: State | ((pendingState: State) => State)) => void];
    declare hook useOptimistic<State, Action>(
      passthrough: State,
      reducer: (state: State, action: Action) => State,
    ): [State, (action: Action) => void];

    /**
     * `useActionState` is a Hook that allows you to update state based on the
     * result of a form action.
     *
     * @see https://react.dev/reference/react/useActionState
     */
    declare hook useActionState<State>(
      action: (state: Promise<State>) => State | Promise<State>,
      initialState: Promise<State>,
      permalink?: string,
    ): [state: Promise<State>, dispatch: () => void, isPending: boolean];
    declare hook useActionState<State, Payload>(
      action: (
        state: Promise<State>,
        payload: Payload,
      ) => State | Promise<State>,
      initialState: Promise<State>,
      permalink?: string,
    ): [
      state: Promise<State>,
      dispatch: (payload: Payload) => void,
      isPending: boolean,
    ];

    type ComponentType<-P: {...}> = component(...P);
    type PropsOf<C: string | React$MixedElement | React$RendersExactly<React$ElementType>> = globalThis.React.PropsOf<C>;
    type PropOf<C: string | React$MixedElement | React$RendersExactly<React$ElementType>, K: string> = globalThis.React.PropOf<C, K>;
    type RefOf<C: string | React$MixedElement | React$RendersExactly<React$ElementType>> = globalThis.React.RefOf<C>;
    type MixedElement = React$MixedElement;
    type ElementType = React$ElementType;

    type Key = React$Key;
    type RefSetter<-T> = React$RefSetter<T>;
    type Node = React$Node;
    type Context<T> = React$Context<T>;
    type Portal = React$Portal;

    type ElementProps<C> = React$ElementProps<C>;
    type ElementConfig<C> = React$ElementConfig<C>;
    type ElementRef<C> = React$ElementRef<C>;

    type ChildrenArray<+T> = $ReadOnlyArray<ChildrenArray<T>> | T;
    type ReactSetStateFunction<S> = ((S => S) | S) => void;
    type RefObject<T> = React$RefObject<T>;
    type Interaction = {
      name: string,
      timestamp: number,
      ...
    };
  };
  declare export default typeof React;
}

type React$ElementRef<C: string | component(...empty)> =
  C extends empty ? any :
  C extends (infer S extends string) ? $JSXIntrinsics[S]['instance'] :
  C extends component(ref: React$RefSetter<infer I>, ...empty) ? I : empty;

// We only keep a bare-bone jsx intrinsics definition. Everything else is moved to flow-typed
declare type $JSXIntrinsics = {
  [string]: {
    instance: any,
    props: any,
    ...
  },
};
