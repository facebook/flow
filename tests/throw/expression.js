// @flow

function x(a: number = throw 'x') {
  ;(a: string) // error
}

x()
x(1)

function f(): number {
  return throw new Error(); // OK to not return
}

function g(a: ?number): number {
  const x = a == null ? throw new Error() : null;
  return a * 1; // a is not null
}
function h(a: ?number): number {
  const x = a == null && throw new Error();
  return a * 1; // TODO: a is not null
}
function i(a: ?number): number {
  const x = a != null || throw new Error();
  return a * 1; // TODO: a is not null
}
