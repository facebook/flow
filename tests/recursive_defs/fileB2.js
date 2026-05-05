// @flow

import type {PropsA} from './fileB1';

export type PropsB = Readonly<{
  ...PropsA,
  propB: string,
  ...
}>;
