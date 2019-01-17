// @flow

import React from 'react';

{
  // Accepts any type of value
  React.useDebugValue('abc');
  React.useDebugValue(123);
  React.useDebugValue(true);
  React.useDebugValue(['a','b','c']);
  React.useDebugValue({foo: 1, bar: 2});
}

{
  // Has an undefined return type
  ((React.useDebugValue(123)): typeof undefined);
}

{
  // Supports optional formatting function
  const date = new Date();
  React.useDebugValue(
    date,
    date => date.toDateString(),
  );
}