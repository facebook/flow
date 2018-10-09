//@flow

class A<T> {}

const a = new A<_>(); // Error, not yet supported (but parses as implicit instantiation).

declare function test<T>(): T;
const b = test<_>(); // Error, not yet supported (but parses as implicit instantiation).
