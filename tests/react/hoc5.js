/**
 * @format
 * @flow
 */

import * as React from 'react';

function hoc<Props, Component: React.ComponentType<Props>>(
  WrappedComponent: Component,
): React.ComponentType<React.ElementConfig<Component>> {
  return (props: Props) => <WrappedComponent {...props} />;
}

class MyComponent1 extends React.Component<{foo: string, bar: number}> {
  static defaultProps: {foo: string} = {foo: 'qux'};
  render(): React.Node {
    return null;
  }
}

<MyComponent1 />; // Error
<MyComponent1 bar={42} />; // OK
<MyComponent1 bar="nope" />; // Error
<MyComponent1 bar={42} foo="zub" />; // OK
<MyComponent1 bar={42} foo={100} />; // Error
<MyComponent1 bar={42} foo={undefined} />; // OK

const MyEnhancedComponent1 = hoc(MyComponent1);

<MyEnhancedComponent1 />; // Error
<MyEnhancedComponent1 bar={42} />; // OK
<MyEnhancedComponent1 bar="nope" />; // Error
<MyEnhancedComponent1 bar={42} foo="zub" />; // OK
<MyEnhancedComponent1 bar={42} foo={100} />; // Error
<MyEnhancedComponent1 bar={42} foo={undefined} />; // OK
