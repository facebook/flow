/* @flow */

var foo = "foo";
var bar = "bar";

var React = require('react');
var Example = React.createClass({
  propTypes: {
    literal: React.PropTypes.oneOf(["foo"]).isRequired,
    string: React.PropTypes.oneOf([foo]).isRequired
  },
});

var ok = <Example literal={foo} string={foo} />;

var fail_wrong_literal = <Example literal={bar} string={foo} />;
var fail_wrong_string = <Example literal={foo} string={bar} />;
