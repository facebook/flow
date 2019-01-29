// @flow

import React from 'react';

{
  React.useEffect(); // Error: function requires another argument.
}

{
  // Ok variants without cleanup functions
  React.useEffect(() => {});
  React.useEffect(() => {}, []);
  React.useEffect(() => {}, [1, 2, 3]);

  // Ok variants with cleanup functions
  React.useEffect(() => () => {});
  React.useEffect(() => () => {}, []);
  React.useEffect(() => () => {}, [1, 2, 3]);
}

{
  React.useEffect(1); // Error: number is incompatible with function type
  React.useEffect(() => {}, 1); // Error: number is incompatible with function react-only array
}
