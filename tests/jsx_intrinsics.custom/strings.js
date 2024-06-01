/* @flow */

var React = require('react');

var Div = 'div';
var Bad = 'bad';
var Str: string = 'str';

<Div />; // This is fine
<Bad />; // Error: 'bad' not in JSXIntrinsics
<Str />; // Error: string ~> keys of JSXIntrinsics

<div />; // This is not fine
<bad />; // Error: 'bad' not in JSXIntrinsics
<Str />; // Error: string ~> keys of JSXIntrinsics

// TODO: Make this an error
<Div id={42} />; // Not an error but should be eventually
