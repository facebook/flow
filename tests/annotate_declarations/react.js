//@flow
import * as React from 'react';

function Component1 (props: { label: string }) {
  return <div>{props.label}</div>;
}

function Component2 (props: { id: number }) {
  return <div>{props.id}</div>;
}

var component = <Component1 label={"component"} />

if (("some condition": any)) {
  component = <Component2 id={42} />
}
