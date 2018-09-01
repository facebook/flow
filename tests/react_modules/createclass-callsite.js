/* @flow */
var React = require('react');
var Hello = require('./createclass-module');
import type {Node} from 'react';

var HelloLocal = React.createClass({
  propTypes: {
    name: React.PropTypes.string.isRequired,
  },

  render: function() {
    return <div>{this.props.name}</div>;
  }
});

var Callsite = React.createClass({
  render: function() {
    return (
      <div>
        <Hello />
        <HelloLocal />
      </div>
    );
  }
});

module.exports = Callsite;
