function test(this: interface {length: number}, a: string, b: string): number {
  return this.length; // expect []/"" this
}

test.apply("", "foo"); // ERROR
declare class MyArrayLike<T> {
  +[index: number]: T;
  @@iterator(): Iterator<T>;
  length: number;
}
var x = new MyArrayLike<string>();
test.apply([], x); // OK
var y = new MyArrayLike<number>();
test.apply("", y); // ERROR

function * gen() {
  yield "foo";
  yield "bar";
}

test.apply([], gen()); // error: iterable ~> array-like
