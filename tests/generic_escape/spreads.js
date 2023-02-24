// @flow

var esc; // error
function hh<X: {}, Y: {}>(x: X, y: Y) {
  esc = {...x, ...y};
}

(esc: void);
