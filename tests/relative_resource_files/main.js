// @flow

var d3_svg = require('./d3.svg'); // no error - should resolve to d3.svg.js
var d3_css = require('./d3.css'); // error - missing resource file
var d3_mov = require('./d3.mov'); // error - existing non-resource file
var d3_tar = require('./d3.tar'); // error - missing non-resource file

(d3_svg.x: string); // error number ~> string
