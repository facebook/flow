// @flow

const React = require("react");

type Props<T> = {|
  t: T,
  f: number
|};

const Component = <T>(props: Props<T>): React.Node => <div />;

Component.defaultProps = {
  f: 1
};

module.exports = Component;
