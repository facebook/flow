//@flow

const React = require('react');

class Component extends React.Component<{}> {}

(Component: React$AbstractComponent<any, any, any>);
(Component: React$AbstractComponent<{}, any, any>);
(Component: React$AbstractComponent<{+foo: number}, any, any>); // Extra props is ok
(Component: React$AbstractComponent<any, {}, any>); // Error no defaults
(Component: React$AbstractComponent<any, {| foo: number |}, any>); // Error extra default props

class ComponentNarrower extends React.Component<{foo: number, bar: number}> {
  static defaultProps: { foo: number } = {foo: 3};
}

(ComponentNarrower: React$AbstractComponent<any, any, any>);
(ComponentNarrower: React$AbstractComponent<{+foo?: number, +bar: number}, {foo: number}, any>);
(ComponentNarrower: React$AbstractComponent<{+foo?: number, +bar: number}, {|foo: number|}, any>); // Error inexact vs exact
(ComponentNarrower: React$AbstractComponent<{}, any, any>); // Error missing foo and bar in config
(ComponentNarrower: React$AbstractComponent<{+foo?: number}, any, any>); // Error missing bar in config
(ComponentNarrower: React$AbstractComponent<any, any, Component>); // Error instance type is wrong
(ComponentNarrower: React$AbstractComponent<any, any, ComponentNarrower>);
(ComponentNarrower: React$AbstractComponent<any, {}, any>); // Error, missing foo in default config
(ComponentNarrower: React$AbstractComponent<any, { foo: number, bar: number }, any>); // Error, bar is not in default config

class Subclass extends Component {}

(Subclass: React$AbstractComponent<any, any, Component>); // Error, Instance is covariant
(Component: React$AbstractComponent<any, any, Subclass>); // Ok, Instance is covariant
(Subclass : React$AbstractComponent<any, any, Subclass>);
