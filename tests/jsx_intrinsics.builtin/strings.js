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

<div {...empty_exact} />;  // This is fine
<bad {...empty_exact} />;  // This is fine

<Div id='str' />; // This is fine
