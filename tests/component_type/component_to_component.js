//@flow
const React = require('react');

class Component extends React.Component<{...}> {}
class Subcomponent extends Component {}

function test1(
  x: component(ref: React.RefSetter<Component>, ...{...}),
): component(ref: React.RefSetter<Component>, ...{...}) { // Ok, all unify
  return x;
}

function test2(
  x: component(ref: React.RefSetter<Component>, ...{...}),
): component(ref: React.RefSetter<Component>, foo: number, ...{...}) { // Extra props is ok
  return x;
}

function test3(
  x: component(ref: React.RefSetter<Component>, foo: number, ...{...}),
): component(ref: React.RefSetter<Component>, ...{...}) { // Error missing props
  return x;
}

function test4(
  x: component(ref: React.RefSetter<Component>, ...{...}),
): component(ref: React.RefSetter<Subcomponent>, ...{...}) { // Error instance is covariant
  return x;
}

function test5(
  x: component(ref: React.RefSetter<Subcomponent>, ...{...}),
): component(ref: React.RefSetter<Component>, ...{...}) { // Ok, instance is covariant
  return x;
}
