// @flow
import React from 'react';
import stylex from './stylex';

const styles = stylex.create({
  foo: { color: 'red' },
});

// Uppercase elements are NOT desugared — sx is treated as a regular prop.
function MyComponent(props: {name: string}): React.Node {
  return null;
}

<MyComponent name="hello" sx={[styles.foo]} />; // ERROR — sx is not in MyComponent's props
