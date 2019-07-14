// @flow

declare class MyArray<T> {
  // TODO: support methods
  +join: <U: number | string>(this: MyArray<U>, separator?: string) => string;
}

declare var foo: MyArray<number>;

foo.join(',') // ok

declare var bar: MyArray<{| a: number |}>;

bar.join(',') // error, object ~> number | string
