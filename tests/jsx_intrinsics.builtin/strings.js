/* @flow */

var React = require('react');

// The builtin $JSXIntrinsics should allow any string

var Div = 'div';
var Bad = 'bad';
var Str: string = 'str';

const empty_exact: {||} = {...null};

<Div />; // This is fine
<Bad />; // This is fine
<Str />; // This is fine

React.createElement('div', empty_exact); // This is fine
React.createElement('bad', empty_exact); // This is fine

<Div id={42} />; // This is fine
