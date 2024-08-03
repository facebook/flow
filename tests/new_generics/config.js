//@flow

import * as React from 'react';
type Props = {foo: number, bar: number};
type DefaultProps = {foo: number};
type Config = {+foo?: number, +bar: number};
type NotTheRightConfig = {+baz: number, +qux: number};

function f<P: Props, D: DefaultProps, C: Config, W: NotTheRightConfig>(
  x: C,
  y: React.Config<P, D>,
  z: W,
) {
  x as React.Config<Props, DefaultProps>;
  y as Config;
  y as C; // error, generics don't match
  y as React.Config<P, D>;
  y as NotTheRightConfig; // Error, configs don't match
  z as React.Config<Props, DefaultProps>; // Error, configs don't match
}
