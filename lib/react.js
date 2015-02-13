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

declare class ReactClass<A,P,S> {
    propTypes: $Record<P>;
    contextTypes: any;
    childContextTypes: any;
    displayName: string;
    defaultProps: $Supertype<P>;
}

declare class ReactComponent<A,P,S> {
    props: P;
    state: S;
    refs: any;
    context: any;

    getInitialState(): S;
    getChildContext(): any;

    setProps(props: $Shape<P>, callback?: () => void): void;
    replaceProps(props: P, callback?: () => void): void;

    setState(state: $Shape<S>, callback?: () => void): void;
    replaceState(state: S, callback?: () => void): void;

    render(): ?ReactElement<any,any,any>;

    forceUpdate(callback?: () => void): void;
    getDOMNode(): any;
    componentWillMount(): void;
    componentDidMount(component?: any): void;
    componentWillReceiveProps(nextProps: P): void;
    shouldComponentUpdate(nextProps: P, nextState?: S, nextContext?: any): boolean;
    componentWillUpdate(nextProps: P, nextState?: S, nextContext?: any): void;
    componentDidUpdate(nextProps: P, nextState?: S, nextContext?: any, component?: any): void;
    componentWillUnmount(): void;
    isMounted(): bool;
}

declare class ReactElement<A,P,S> {
    type: ReactClass<A,P,S>;
    props: P;
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
  oneOf: (expectedValues: Array) => ReactPropsChainableTypeChecker;
  oneOfType: (arrayOfTypeCheckers: Array<ReactPropsCheckType>) => ReactPropsChainableTypeChecker;
  shape: (shapeTypes: { [key: string]: ReactPropsCheckType }) => ReactPropsChainableTypeChecker;
}

declare module react {
    declare var Children: any;
    declare var DOM: any;
    declare var PropTypes: ReactPropTypes;

    declare function initializeTouchEvents(shouldUseTouch: boolean): void;

    declare function createClass(spec: any): ReactClass<any,any,any>; // compiler magic

//    declare function createElement(name: string, props?: any, children?: any): any;
    declare function createElement<A,P,S>(name: ReactClass<A,P,S>, props: A & $Shape<P>, children?: any): ReactElement<A,P,S>;

    declare function createFactory(name: string): (props?: any, children?: any) => any;
    declare function createFactory<A,P,S>(name: ReactClass<A,P,S>): (props: A, children?: any) => ReactElement<A,P,S>;

    declare function constructAndRenderComponent(name: string, props: any, container: any): any;
    declare function constructAndRenderComponent<A,P,S>(name: ReactClass<A,P,S>, props: A, container: any): ReactComponent<A,P,S>;

    declare function constructAndRenderComponentByID(name: string, props: any, id: string): any;
    declare function constructAndRenderComponentByID<A,P,S>(name: ReactClass<A,P,S>, props: A, id: string): ReactComponent<A,P,S>;

    declare function render<A,P,S>(element: ReactElement<A,P,S>, container: any): ReactComponent<A,P,S>;

    declare function renderToString(element: ReactElement<any,any,any>): string;
    declare function renderToStaticMarkup(element: ReactElement<any,any,any>): string;

    declare function unmountComponentAtNode(container: any): boolean;

    declare function isValidElement(element: any): boolean;
    declare function withContext(context: any, callback: () => void): any;
}

// TODO Delete this once https://github.com/facebook/react/pull/3031 lands
// and "react" becomes the standard name for this module
declare module React {
    declare var Children: any;
    declare var DOM: any;
    declare var PropTypes: ReactPropTypes;

    declare function initializeTouchEvents(shouldUseTouch: boolean): void;

    declare function createClass(spec: any): ReactClass<any,any,any>; // compiler magic

//    declare function createElement(name: string, props?: any, children?: any): any;
    declare function createElement<A,P,S>(name: ReactClass<A,P,S>, props: A & $Shape<P>, children?: any): ReactElement<A,P,S>;

    declare function createFactory(name: string): (props?: any, children?: any) => any;
    declare function createFactory<A,P,S>(name: ReactClass<A,P,S>): (props: A, children?: any) => ReactElement<A,P,S>;

    declare function constructAndRenderComponent(name: string, props: any, container: any): any;
    declare function constructAndRenderComponent<A,P,S>(name: ReactClass<A,P,S>, props: A, container: any): ReactComponent<A,P,S>;

    declare function constructAndRenderComponentByID(name: string, props: any, id: string): any;
    declare function constructAndRenderComponentByID<A,P,S>(name: ReactClass<A,P,S>, props: A, id: string): ReactComponent<A,P,S>;

    declare function render<A,P,S>(element: ReactElement<A,P,S>, container: any): ReactComponent<A,P,S>;

    declare function renderToString(element: ReactElement<any,any,any>): string;
    declare function renderToStaticMarkup(element: ReactElement<any,any,any>): string;

    declare function unmountComponentAtNode(container: any): boolean;

    declare function isValidElement(element: any): boolean;
    declare function withContext(context: any, callback: () => void): any;
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
