//@flow

import * as React from 'react';
type Props = {foo: number, bar: number};
type DefaultProps = {foo: number};
type Config = {+foo?: number, +bar: number};
type NotTheRightConfig = {+baz: number, +qux: number};

function f<P: Props, D: DefaultProps, C: Config, W: NotTheRightConfig>(
  x: C,
  y: React$Config<P, D>,
  z: W,
) {
  (x: React$Config<Props, DefaultProps>);
  (y: Config);
  (y: C); // error, generics don't match
  (y: React$Config<P, D>);
  (y: NotTheRightConfig); // Error, configs don't match
  (z: React$Config<Props, DefaultProps>); // Error, configs don't match
}
