/* @flow */

function a(): number {
  var x: ?string = null;
  return x ? 1 : 0;
}

function b(): number {
    var x: ?number = null;
    return x != null ? x : 0;
}

function c(): number {
  // equivalent to `return (x && 1) || 0`
  const x = false;
  const temp = (x ? 1 : x); // constant-condition error
  return temp ? temp : 0;
}

function d(): string { // expected `: number | boolean`
  // equivalent to `return x != null && x`
  var x: ?number = null;
  return (x != null) ? x : (x != null);
}

type Obj = {
  f?: ?number,
};

function foo(m: ?string): ?Obj {
  return m != null ? JSON.parse(m) : {};
}
