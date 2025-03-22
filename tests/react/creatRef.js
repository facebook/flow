import React from 'react';

{
  class MyComponent extends React.Component<void> {}

  const ref: {current: null | MyComponent} = React.createRef(); // Ok
}

{
  const ref: {|current: null | number|} = React.createRef(); // Ok
}
