/* @flow */

var React = require('react');
var Foo = React.createClass({
  propTypes: {
    foo: React.PropTypes.oneOf(["foo"]),
    bar: React.PropTypes.string.isRequired,
  },
});

var props = {foo: "bar", bar: 42};
var blah = <Foo {...props} />; // error bar, number given string expected
