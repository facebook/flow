// @flow

var React = require('react');

function F(props: {x: number, y: string}) {}
component C(x: number, z: string) { return null; }
declare const cond: boolean;
const U = cond ? F : C;
<U  // space
// ^
