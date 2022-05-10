// @flow

function foo(x) {
  return x(x);
}

  foo(x => x);
// ^
type Foo = { n: typeof foo };
function bar(x: Foo) {}
//               ^
