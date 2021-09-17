// @flow

import * as React from 'react';

function Component (props) {
  return <div>{props.label}</div>;
}

function Parent() {
  const a = Component({label: 'a'});
  return (
    <>
      {a}
      <Component label="b" />
    </>
  )
}
