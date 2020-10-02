// @flow

import React from "react";

class JDiv extends React.Component<{id: string}> {}

// Should be a type error ('id' takes a string, not a number..)
<JDiv id={42} />;

class Example extends React.Component<{ bar: string }> {
  render() {
    return <div>{this.props.bar}</div>
  }
}

<Example foo="foo" />;
