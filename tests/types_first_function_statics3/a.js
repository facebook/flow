// @flow

const React = require("react");

type Props = {|
  f: number
|};

const Component = (props: Props): React.Node => <div />;

Component.defaultProps = {
  f: 1
};

module.exports = Component;
