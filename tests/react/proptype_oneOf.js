/* @flow */

var React = require('react');
var Example = React.createClass({
  propTypes: {
    literal: React.PropTypes.oneOf(["foo"])
  },
});

var ex1 = <Example literal="foo" />;
var ex2 = <Example literal="bar" />;
