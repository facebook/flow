declare module 'react-dom' {
  import {Component as ReactComponent} from 'react';

  declare function findDOMNode(
    componentOrElement: Element | ?ReactComponent<any, any>,
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
    parentComponent: ReactComponent<any, any>,
    nextElement: React$Element<ElementType>,
    container: any,
    callback?: () => void,
  ): React$ElementRef<ElementType>;
}

declare module 'react-dom/server' {
  declare function renderToString(element: React$Node): string;
  declare function renderToStaticMarkup(element: React$Node): string;
  declare function renderToNodeStream(element: React$Node): stream$Readable;
  declare function renderToStaticNodeStream(
    element: React$Node,
  ): stream$Readable;
  declare var version: string;
}

type Thenable = { then(resolve: () => mixed, reject?: () => mixed): mixed, ... };

declare module 'react-dom/test-utils' {
  import {Component as ReactComponent} from 'react';

  declare var Simulate: { [eventName: string]: (element: Element, eventData?: Object) => void, ... };
  declare function renderIntoDocument(
    instance: React.MixedElement,
  ): ReactComponent<any, any>;
  declare function mockComponent(
    componentClass: React$ElementType,
    mockTagName?: string,
  ): Object;
  declare function isElement(element: React$Element<any>): boolean;
  declare function isElementOfType(
    element: React.MixedElement,
    componentClass: React$ElementType,
  ): boolean;
  declare function isDOMComponent(instance: any): boolean;
  declare function isCompositeComponent(
    instance: ReactComponent<any, any>,
  ): boolean;
  declare function isCompositeComponentWithType(
    instance: ReactComponent<any, any>,
    componentClass: React.ElementType,
  ): boolean;
  declare function findAllInRenderedTree(
    tree: ReactComponent<any, any>,
    test: (child: ReactComponent<any, any>) => boolean,
  ): Array<ReactComponent<any, any>>;
  declare function scryRenderedDOMComponentsWithClass(
    tree: ReactComponent<any, any>,
    className: string,
  ): Array<Element>;
  declare function findRenderedDOMComponentWithClass(
    tree: ReactComponent<any, any>,
    className: string,
  ): ?Element;
  declare function scryRenderedDOMComponentsWithTag(
    tree: ReactComponent<any, any>,
    tagName: string,
  ): Array<Element>;
  declare function findRenderedDOMComponentWithTag(
    tree: ReactComponent<any, any>,
    tagName: string,
  ): ?Element;
  declare function scryRenderedComponentsWithType(
    tree: ReactComponent<any, any>,
    componentClass: React$ElementType,
  ): Array<ReactComponent<any, any>>;
  declare function findRenderedComponentWithType(
    tree: ReactComponent<any, any>,
    componentClass: React.ElementType,
  ): ?ReactComponent<any, any>;
  declare function act(callback: () => void | Thenable): Thenable;
}
