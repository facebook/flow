//@flow

const React = require('react');

class Component extends React.Component<{}> {}

Component as React$AbstractComponent<any, any, any>;
Component as React$AbstractComponent<{}, any, any>;
Component as React$AbstractComponent<{+foo: number}, any, any>; // Extra props is ok

class ComponentNarrower extends React.Component<{foo: number, bar: number}> {
  static defaultProps: {foo: number} = {foo: 3};
}

ComponentNarrower as React$AbstractComponent<any, any, any>;
ComponentNarrower as React$AbstractComponent<
  {+foo?: number, +bar: number},
  any,
  any,
>;
ComponentNarrower as React$AbstractComponent<{}, any, any>; // Error missing foo and bar in config
ComponentNarrower as React$AbstractComponent<{+foo?: number}, any, any>; // Error missing bar in config
ComponentNarrower as React$AbstractComponent<any, Component, any>; // Error instance type is wrong
ComponentNarrower as React$AbstractComponent<any, ComponentNarrower, any>;

class Subclass extends Component {}

Subclass as React$AbstractComponent<any, Component, any>; // Error, Instance is covariant
Component as React$AbstractComponent<any, Subclass, any>; // Ok, Instance is covariant
Subclass as React$AbstractComponent<any, Subclass, any>;

class SpecificRender extends React.Component<{}> {
  render(): number {
    return 3;
  }
}

SpecificRender as React$AbstractComponent<{}, SpecificRender, number>;
SpecificRender as React$AbstractComponent<{}, SpecificRender, React$Node>; // Ok, covariant type argument
SpecificRender as React$AbstractComponent<{}, SpecificRender, string>; // Error, number ~> string
