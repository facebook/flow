// @flow

var esc; // error
function hh<X extends {}, Y extends {}>(x: X, y: Y) {
  esc = {...x, ...y};
}

esc as void;
