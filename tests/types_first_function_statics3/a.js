// @flow

const React = require("react");

type Props = {|
  f: number
|};

const Component = (props: Props): React.Node => <div />;

const defaultProps = {
  f: 1
};

module.exports = Component;

module.exports.defaultProps = defaultProps;
