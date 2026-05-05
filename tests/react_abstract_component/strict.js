const React = require('react');

declare var Component : React.ElementType;
const c = <Component f = {3}/>;
c as React.MixedElement;

declare function foo(a: ?React.Component<any, any>): void;
declare var ref: ?React.ElementRef<React.ElementType>;
foo(ref);

class Component2 extends React.Component<{}> {};
const d = <Component2/>;
d as React.MixedElement;

type Props = {someProp : string};
type State = {};

class MyComponent extends React.Component<Props, State> {
  static _cloneElement(
    element: ExactReactElement_DEPRECATED<typeof MyComponent>,
  ): React.MixedElement {
    const someProp = 'some value';
    return React.cloneElement(element, {someProp});
  }
}


declare var e : ExactReactElement_DEPRECATED<Class<Component2>>;
e as React.MixedElement;
