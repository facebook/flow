// @flow

type Foo = {
  bar: {
      baz: ?{
          qux: ReadonlyArray<string>;
      }
  }
}

type T1 = Foo;
//   ^

type T2 = Foo["bar"];
//   ^

type T3 = Foo["bar"]["baz"]
//   ^

type T4 = Foo["bar"]["baz"]?.["qux"];
//   ^

type T5 = Foo["bar"]["baz"]?.["qux"][number];
//   ^

type T6 = NonNullable<Foo["bar"]["baz"]?.["qux"][number]>;
//   ^

type T7 = keyof Foo;
//   ^

type T8 = keyof {foo: string, bar: number, baz: bigint};
//   ^

type T9 = $Omit<{foo: string, bar: number}, 'foo'>;
//   ^

type ValuesPoly<X> = Values<X>
//   ^

type ValuesPoly<X> = Values<X>
//                   ^
