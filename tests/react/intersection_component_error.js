/**
 * @format
 * @flow
 */

import * as React from 'react';

declare const MyComponent: React.ComponentType<{foo: number, ...}> & {
  someStatic: boolean,
  ...
};

<MyComponent />;
