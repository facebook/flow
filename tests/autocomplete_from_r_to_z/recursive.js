// @flow

declare var Foo: React.ComponentType<Props>;

type Props = Readonly<{
  children?: typeof Foo,
  ab: number,
  ac: typeof Foo,
}>;

<Foo  />
//   ^
