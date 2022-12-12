/* @flow */

type thing = number | boolean

function foo(x: thing) {
  if (x === true) {
    x[0]; // error on boolean
  }
}

function bar(x: thing) {
  if (x !== true && x !== false) {
    x[0]; // error on number
  }
}

function baz(x: ?thing) {
  if (x && x !== true) {
    x[0]; // error on number
  }
}
