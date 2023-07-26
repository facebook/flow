//@flow
const React = require('react');

class Component extends React.Component<{}> {}
class Subcomponent extends Component {}

function test1(
  x: React$AbstractComponent<{}, Component, React$Node>,
): React$AbstractComponent<{}, Component, React$Node> { // Ok, all unify
  return x;
}

function test2(
  x: React$AbstractComponent<{}, Component, React$Node>,
): React$AbstractComponent<{foo: number}, Component, React$Node> { // Extra props is ok
  return x;
}

function test3(
  x: React$AbstractComponent<{foo: number}, Component, React$Node>,
): React$AbstractComponent<{}, Component, React$Node> { // Error missing props
  return x;
}

function test4(
  x: React$AbstractComponent<{}, Component, React$Node>,
): React$AbstractComponent<{}, Subcomponent, React$Node> { // Error instance is covariant
  return x;
}

function test5(
  x: React$AbstractComponent<{}, Subcomponent, React$Node>,
): React$AbstractComponent<{}, Component, React$Node> { // Ok, instance is covariant
  return x;
}
