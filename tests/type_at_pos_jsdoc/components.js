//@flow

import React from 'react';

/**
 * This is the component Foo!
 */
component Foo(
  /** doc 1 */
  bar: string,
// ^
  /** doc 2 */
  ...rest: {
//    ^
    /** doc 3 */
    baz: string,
//   ^
  }
) {
  return <div />;
}

<Foo bar="bar" baz="baz" />;

  Foo;
// ^

<Foo
  bar="baz"
// ^
  baz="bar" />;
// ^
