//@flow
import * as React from 'react';

class MyComponent extends React.Component<{| foo: number |}> {
  render(): React.Node {
    return this.props.foo;
  }
}

function wrapper<TProps: {}, TInstance, TRenders: React$Node>(
  base: React$AbstractComponent<TProps, TInstance, TRenders>,
): React$AbstractComponent<TProps, TInstance, TRenders> {
  return base;
}

function wrapper2<TProps: {}, TInstance, TRenders: React$Node>(
  base: React$AbstractComponent<TProps, TInstance, TRenders>,
): React$AbstractComponent<TProps, TInstance, TRenders> {
  return base;
}

const WrappedBoth = wrapper(wrapper2(MyComponent)); // Errors-- props incompatible with component
const _a = <WrappedBoth foo={42} bar={43} />; // Error, extra prop bar
const _b = <WrappedBoth />; // Error, missing prop foo
const _c = <WrappedBoth foo={42} />;
