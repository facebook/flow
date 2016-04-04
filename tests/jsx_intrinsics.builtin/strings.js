/* @flow */

var React = require('react');

// The builtin $JSXIntrinsics should allow any string

var Div = 'div';
var Bad = 'bad';

<Div />; // This is fine
<Bad />; // This is fine

React.createElement('div', {}); // This is fine
React.createElement('bad', {}); // This is fine

<Div id={42} />; // This is fine
