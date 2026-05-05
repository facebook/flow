import * as React from 'react';

declare opaque type T;
type Props = {t: T};
declare var props: Props;

type D<Context: {+context: unknown, ...}, Props> = Omit<
  Props,
  'context'
>;
type X<Context: {+context: unknown, ...}, Props> = {
  x: D<Context, Props>,
};
class Foo<
  Props: {},
  Context: {+context: unknown, ...},
> extends React.Component<
  X<Context, Props>
> {}
//Error: cannot create Foo
<Foo x={props.t} />;

type Y<Context: {+context: unknown, ...}, Props> = {
  y: Omit<Props, 'context'>;
};
class Bar<
  Props: {+context: unknown, ...},
  Context: {},
> extends React.Component<
  Y<Context, Props>
> {}
//Error: cannot create Bar
<Bar y={props.t} />;
