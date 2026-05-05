import * as React from 'react';

declare opaque type T;
type Props = {t: T};
declare var props: Props;

type D<Context extends {+context: unknown, ...}, Props> = Omit<
  Props,
  'context'
>;
type X<Context extends {+context: unknown, ...}, Props> = {
  x: D<Context, Props>,
};
class Foo<
  Props extends {},
  Context extends {+context: unknown, ...},
> extends React.Component<
  X<Context, Props>
> {}
//Error: cannot create Foo
<Foo x={props.t} />;

type Y<Context extends {+context: unknown, ...}, Props> = {
  y: Omit<Props, 'context'>;
};
class Bar<
  Props extends {+context: unknown, ...},
  Context extends {},
> extends React.Component<
  Y<Context, Props>
> {}
//Error: cannot create Bar
<Bar y={props.t} />;
