/* @flow */

function foo(x:{y?:() => void}) {
  x.y(); // could be undefined
  if (x.hasOwnProperty('y')) {
    x.y(); // ok
  }
  if (x.hasOwnProperty('z')) {
    x.z(); // unreachable, so allowed
  }
}

function bar(x:Object) {
  x.y(); // unknown, so allowed
  if (x.hasOwnProperty('y')) {
    x.y(); // ok
  }
  if (x.hasOwnProperty('z')) {
    x.z(); // ok
  }
}
