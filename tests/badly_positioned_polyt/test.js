import * as React from 'react';

declare opaque type T;
type Props = {t: T};
declare var props: Props;

type D<Context: {+context: mixed, ...}, Props> = Omit<
  Props,
  'context'
>;
type X<Context: {+context: mixed, ...}, Props> = {
  x: D<Context, Props>,
};
class Foo<
  Props: {},
  Context: {+context: mixed, ...},
> extends React.Component<
  X<Context, Props>
> {}
//Error: cannot create Foo
<Foo x={props.t} />;

type Y<Context: {+context: mixed, ...}, Props> = {
  y: Omit<Props, 'context'>;
};
class Bar<
  Props: {+context: mixed, ...},
  Context: {},
> extends React.Component<
  Y<Context, Props>
> {}
//Error: cannot create Bar
<Bar y={props.t} />;
