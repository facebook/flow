// @flow

declare var Foo: React$ComponentType<Props>;

type Props = $ReadOnly<{
  children?: typeof Foo,
  ab: number,
  ac: typeof Foo,
}>;

<Foo  />
//   ^
