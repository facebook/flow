/* @flow */
var React = require('react');
import type {Node} from 'react';

const Hello: Class<React$Component<{ name: string }, {}>> = React.createClass({
  propTypes: {
    name: React.PropTypes.string.isRequired,
  },

  render: function(): Node {
    return <div>{this.props.name}</div>;
  }
});

module.exports = Hello;
