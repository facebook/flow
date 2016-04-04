/* @flow */

var React = require('react');

var Div = 'div';
var Bad = 'bad';

<Div />; // This is fine
<Bad />; // Error: 'bad' not in JSXIntrinsics

React.createElement('div', {}); // This is fine
React.createElement('bad', {}); // Error: 'bad' not in JSXIntrinsics

// TODO: Make this an error
<Div id={42} />; // Not an error but should be eventually
