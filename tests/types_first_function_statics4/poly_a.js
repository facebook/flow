// @flow

function foo<T>() {}
foo.x = 1;

const bar = <T>() => {};
bar.x = 1;

const React = require("react");

type Props<T> = {|
  t: T,
  f: number
|};

const defaultProps = {
  f: 1
};

function FooComponent<T>(props: Props<T>): React.Node { return <div />; }
FooComponent.defaultProps = defaultProps;

const BarComponent = <T>(props: Props<T>): React.Node => <div />;
BarComponent.defaultProps = defaultProps;

module.exports = { foo, bar, FooComponent, BarComponent };
