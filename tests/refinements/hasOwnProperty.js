/* @flow */

function foo(x:{y?:() => void}) {
  x.y(); // could be undefined
  if (x.hasOwnProperty('y')) {
    x.y(); // ok
  }
  if (x.hasOwnProperty('z')) {
    x.z(); // doesn't exist
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
