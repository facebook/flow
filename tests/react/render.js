import * as React from 'react';

/**
 * Basic render: Class component tests
 */
class A extends React.Component<{}, void> {
  render = (): React.Node => {};
}
class B extends React.Component<{}, void> {
  render = (): React.Node => null;
}

<A />;
// OK

<B />;
// OK

/**
 * Basic render: stateless functional component tests
 */
const C: React.ComponentType<{}> = props => {};
const D: React.ComponentType<{}> = props => {
  return;
};
const E: React.ComponentType<{}> = props => {
  return null;
};

<C foo="bar" />;
// Error: `React$AbstractComponent` must return `React$Node` which can't be undefined

<D foo="bar" />;
// Error: `React$AbstractComponent` must return `React$Node` which can't be undefined

<E foo="bar" />;
// OK

type UnfixedPropsType = {|foo?: string|};

/**
 * PropsType inference: Class component test
 */
class F extends React.Component<UnfixedPropsType, void> {
  render = (): React.Node => this.props.foo;
}
// Error: props.foo could be undefined

/**
 * PropsType inference: stateless functional component test
 */
const G: React.ComponentType<UnfixedPropsType> = props => {
  return props.foo;
};
// Error: props.foo could be undefined
