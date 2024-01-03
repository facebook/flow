// @flow

import type {PropsA} from './fileA1';

export type PropsB = $ReadOnly<{
  ...PropsA,
  propB: string,
  ...
}>;
