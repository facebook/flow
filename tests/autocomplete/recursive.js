// @flow

declare var Foo: React$AbstractComponent<Props, any>;

type Props = $ReadOnly<{
  children?: typeof Foo,
  ab: number,
  ac: typeof Foo,
}>;

<Foo  />
//   ^
