// @flow

import React from "react";
import ReactDOM from "react-dom";

class JDiv extends React.Component<void, {id: string}> {
  // static defaultProps: { };
}

// Should be a type error ('id' takes a string, not a number..)
<JDiv id={42} />;

class Example extends React.Component<void, {bar: string}> {
  render() {
    return <div>{this.props.bar}</div>
  }
}

ReactDOM.render(
  <Example foo="foo" />,
  document.body
);
