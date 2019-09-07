/* @flow */

function highlander(howMany: 1): number {
  return howMany; // there can be only one!
}

highlander(1);
highlander(2); // error


type Foo = 1 | 2

function bar(num: Foo): number {
  return num + 1;
}

bar(1);
bar(2);
bar(3); // error

type ComparatorResult = -1 | 0 | 1
function sort(fn: (x: any, y: any) => ComparatorResult) {}
sort((x, y) => -1);

function constLiteral() {
  const foo = 1;
  const foo_check: typeof foo = 2;
  const mutable_foo = {
    foo,
    method() {
      mutable_foo.foo = 'str';
    }
  };
  (mutable_foo.foo: number); // error
}
