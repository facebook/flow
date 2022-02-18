// @flow

var esc;
function hh<X: {}, Y: {}>(x: X, y: Y) {
  esc = {...x, ...y};
}
