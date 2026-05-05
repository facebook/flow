// @flow

import type {PropsA} from './fileA1';

export type PropsB = Readonly<{
  ...PropsA,
  propB: string,
  ...
}>;
