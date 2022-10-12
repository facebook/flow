// @flow

const React = require('react');

declare function A({foo: string}): React.Node;
declare function B({bar: string}): React.Node;

function f(b: boolean) {
  if (b) {
    return <A foo="hey"/>;
  } else {
    return <B bar="hey"/>;
  }
}
