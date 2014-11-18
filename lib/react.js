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

    setProps(props: $Supertype<P>, callback?: () => void): void;
    replaceProps(props: P, callback?: () => void): void;

    setState(state: $Supertype<S>, callback?: () => void): void;
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
}

declare class ReactElement<A,P,S> {
    type: ReactClass<A,P,S>;
    props: P;
    key: ?string;
    ref: any;
}

declare var React: {
    Children: any;
    DOM: any;
    PropTypes: any;

    initializeTouchEvents(shouldUseTouch: boolean): void;

    createClass(spec: any): ReactClass<any,any,any>; // compiler magic

    //createElement(name: string, props?: any, children?: any): any;
    createElement<A,P,S>(name: ReactClass<A,P,S>, props: A, children?: any): ReactElement<A,P,S>;

    createFactory(name: string): (props?: any, children?: any) => any;
    createFactory<A,P,S>(name: ReactClass<A,P,S>): (props: A, children?: any) => ReactElement<A,P,S>;

    constructAndRenderComponent(name: string, props: any, container: any): any;
    constructAndRenderComponent<A,P,S>(name: ReactClass<A,P,S>, props: A, container: any): ReactComponent<A,P,S>;

    constructAndRenderComponentByID(name: string, props: any, id: string): any;
    constructAndRenderComponentByID<A,P,S>(name: ReactClass<A,P,S>, props: A, id: string): ReactComponent<A,P,S>;

    render<A,P,S>(element: ReactElement<A,P,S>, container: any): ReactComponent<A,P,S>;

    renderToString(element: ReactElement<any,any,any>): string;
    renderToStaticMarkup(element: ReactElement<any,any,any>): string;

    unmountComponentAtNode(container: any): boolean;

    isValidElement(element: any): boolean;
    withContext(context: any, callback: () => void): any;
}
