// @flow

import * as React from 'react';

function connect<TProps>(
  Component: React.ComponentType<{|...TProps, prop: number|}>,
): React.ComponentType<TProps> {
  return (null: any);
}

class MyComponent extends React.Component<{|
  a: number,
  b: number,
  prop: number,
|}> {}

function MyFunctionComponent(props: {|
  a: number,
  b: number,
  prop: number,
|}) {}

const MyEnhancedComponent = connect(MyComponent);
const MyEnhancedFunctionComponent = connect(MyFunctionComponent);

<MyEnhancedComponent />; // Error: Needs `a` and `b`.
<MyEnhancedComponent a={1} b={2} />; // OK
<MyEnhancedComponent a="foo" b={2} />; // Error: string ~> number

<MyEnhancedFunctionComponent/>; // Error: Needs `a` and `b`.
<MyEnhancedFunctionComponent a={1} b={2} />; // OK
<MyEnhancedFunctionComponent a="foo" b={2} />; // Error: string ~> number
