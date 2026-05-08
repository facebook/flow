/* @flow */

var React = require('react');

var Div = 'div' as const;
var Bad = 'bad' as const;
var Str: string = 'str';

<Div id="foo" />; // This is fine
<Bad />; // Error: 'bad' not in JSXIntrinsics
<Str />; // Error: string ~> keys of JSXIntrinsics

<div />; // This is not fine
<bad />; // Error: 'bad' not in JSXIntrinsics
<Str />; // Error: string ~> keys of JSXIntrinsics

<Div id={42} />; // Error: 42 ~> string
