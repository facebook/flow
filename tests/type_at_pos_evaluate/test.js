// @flow

type Foo = {
  bar: {
      baz: ?{
          qux: $ReadOnlyArray<string>;
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

type T6 = $NonMaybeType<Foo["bar"]["baz"]?.["qux"][number]>;
//   ^

type T7 = $Keys<Foo>;
//   ^

type T8 = $Keys<{foo: string, bar: number, baz: bigint}>;
//   ^
