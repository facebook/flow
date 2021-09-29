// @flow

function foo1(x: Array<number>): [number, ?number] {
  return x; // Error, can't enforce arity when flowing array to tuple
}

function foo2(x: Array<number>): [number, ?number] {
  return [x[0], x[1]]; // OK. This is unsound, but at least arity is enforced
}
