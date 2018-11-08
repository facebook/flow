// @flow

import React from 'react';

{
  React.useReducer(); // Error: function requires another argument.
}

{
  const [state, dispatch] = React.useReducer(
    (state, action) => action.value,
    1
  );
  dispatch({value: 2});

  (state: number); // Ok
  (state: string); // Error: number is incompatible with string
}