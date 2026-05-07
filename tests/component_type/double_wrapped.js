//@flow
import * as React from 'react';

class MyComponent extends React.Component<{ foo: number }> {
  render(): React.Node {
    return this.props.foo;
  }
}

function wrapper<TProps extends {...}, TRenders extends React$Node>(
  base: component(...TProps) renders TRenders,
): component(...TProps) renders TRenders {
  return base;
}

function wrapper2<TProps extends {...}, TInstance, TRenders extends React$Node>(
  base: component(...TProps) renders TRenders,
): component(...TProps) renders TRenders {
  return base;
}

const WrappedBoth = wrapper(wrapper2(MyComponent)); // Errors-- props incompatible with component
const _a = <WrappedBoth foo={42} bar={43} />; // Error, extra prop bar
const _b = <WrappedBoth />; // Error, missing prop foo
const _c = <WrappedBoth foo={42} />;
