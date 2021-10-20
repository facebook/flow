// @flow

type T = number;

declare export class Foo extends Qux<string> mixins Bar<number> {
}
declare class Bar<T> extends Baz<T> {
  y: T
}
declare class Qux<T> extends Baz<T> {
  y: T, z: T
}
declare class Baz<T> {
  x: T
}


declare class D {}

declare opaque type O1;
opaque type O2 = typeof D;

type Number = number;

// $FlowExpectedError[type-as-value]
declare export class C mixins O1, O2, Number {}
