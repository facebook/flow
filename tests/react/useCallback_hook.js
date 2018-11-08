// @flow

import React from 'react';

{
  React.useCallback(); // Error: function requires another argument.
}

{
  let numeric: number;
  numeric = React.useCallback(() => 123); // Ok
  numeric = React.useCallback(() => 123, []); // Ok
  numeric = React.useCallback(() => 123, [1, 2, 3]); // Ok
  numeric = React.useCallback(() => 'abc'); // Error: is incompatible with number
  numeric = React.useCallback(() => 123, {}); // Error: object literal is incompatible with +read-only array type
}