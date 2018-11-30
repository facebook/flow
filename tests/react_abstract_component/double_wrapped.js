//@flow
import * as React from 'react';

class MyComponent extends React.Component<{| foo: number |}> {
  render() {
    return this.props.foo;
  }
}

function wrapper<TProps: {}, TDefaultProps: ?{}, TInstance>(
  base: React$AbstractComponent<TProps, TDefaultProps, TInstance>,
): React$AbstractComponent<TProps, TDefaultProps, TInstance> {
  return base;
}

function wrapper2<TProps: {}, TDefaultProps: ?{}, TInstance>(
  base: React$AbstractComponent<TProps, TDefaultProps, TInstance>,
): React$AbstractComponent<TProps, TDefaultProps, TInstance> {
  return base;
}

const WrappedBoth = wrapper(wrapper2(MyComponent));
const _a = <WrappedBoth foo={42} bar={43} />; // Error, extra prop bar
const _b = <WrappedBoth />; // Error, missing prop foo
