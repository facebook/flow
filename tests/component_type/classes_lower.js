//@flow

const React = require('react');

class Component extends React.Component<{...}> {}

Component as component(ref: React.RefSetter<any>, ...any);
Component as component(ref: React.RefSetter<any>);
Component as component(ref: React.RefSetter<any>, foo: number); // Extra props is ok

class ComponentNarrower extends React.Component<{foo: number, bar: number, ...}> {
  static defaultProps: {foo: number} = {foo: 3};
}

ComponentNarrower as component(ref: React.RefSetter<any>, ...any);
ComponentNarrower as component(ref: React.RefSetter<any>, bar: number, foo?: number, ...{...}) ;
ComponentNarrower as component(ref: React.RefSetter<any>, ...{...}); // Error missing foo and bar in config
ComponentNarrower as component(ref: React.RefSetter<any>, foo?: number); // Error missing bar in config
ComponentNarrower as component(ref: React.RefSetter<Component>, ...{...}); // Error instance type is wrong
ComponentNarrower as component(ref: React.RefSetter<ComponentNarrower>, foo?: number, bar: number, ...{...});

class Subclass extends Component {}

Subclass as component(ref: React.RefSetter<Component>, ...any); // Error, Instance is covariant
Component as component(ref: React.RefSetter<Subclass>, ...any); // Ok, Instance is covariant
Subclass as component(ref: React.RefSetter<Subclass>, ...any);

class SpecificRender extends React.Component<{...}> {
  render(): number {
    return 3;
  }
}

SpecificRender as component(ref: React.RefSetter<SpecificRender>, ...{...}); // Ok, covariant type argument
