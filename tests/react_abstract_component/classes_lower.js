//@flow

const React = require('react');

class Component extends React.Component<{}> {}

(Component: React$AbstractComponent<any, any, any>);
(Component: React$AbstractComponent<{}, any, any>);
(Component: React$AbstractComponent<{+foo: number}, any, any>); // Extra props is ok

class ComponentNarrower extends React.Component<{foo: number, bar: number}> {
  static defaultProps: { foo: number } = {foo: 3};
}

(ComponentNarrower: React$AbstractComponent<any, any, any>);
(ComponentNarrower: React$AbstractComponent<{+foo?: number, +bar: number}, any, any>);
(ComponentNarrower: React$AbstractComponent<{}, any, any>); // Error missing foo and bar in config
(ComponentNarrower: React$AbstractComponent<{+foo?: number}, any, any>); // Error missing bar in config
(ComponentNarrower: React$AbstractComponent<any, Component, any>); // Error instance type is wrong
(ComponentNarrower: React$AbstractComponent<any, ComponentNarrower, any>);

class Subclass extends Component {}

(Subclass: React$AbstractComponent<any, Component, any>); // Error, Instance is covariant
(Component: React$AbstractComponent<any, Subclass, any>); // Ok, Instance is covariant
(Subclass : React$AbstractComponent<any, Subclass, any>);

class SpecificRender extends React.Component<{}> {
  render(): number {
    return 3;
  }
}

(SpecificRender: React$AbstractComponent<{}, SpecificRender, number>);
(SpecificRender: React$AbstractComponent<{}, SpecificRender, React$Node>); // Ok, covariant type argument
(SpecificRender: React$AbstractComponent<{}, SpecificRender, string>); // Error, number ~> string
