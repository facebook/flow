// @flow

import React from 'react';

{
  React.useImperativeMethods(); // Error: function requires another argument.
}

type Interface = {|
  focus: () => void
|};

{
  const api: Interface = {
    focus: () => {}
  };

  const ref: {current: null | Interface } = React.createRef();
  React.useImperativeMethods(ref, () => api); // Ok

  const refSetter = (instance: null | Interface) => {};
  React.useImperativeMethods(refSetter, () => api); // Ok
}

{
  const api: Interface = {
    focus: () => {}
  };

  const ref: {current: null | Interface } = React.createRef();
  React.useImperativeMethods(ref, () => ({})); // Error: inexact object literal is incompatible with exact Interface

  const refSetter = (instance: null | Interface) => {};
  React.useImperativeMethods(refSetter, () => ({})); // Error: inexact object literal is incompatible with exact Interface
}
