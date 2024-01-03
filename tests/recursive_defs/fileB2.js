// @flow

import type {PropsA} from './fileB1';

export type PropsB = $ReadOnly<{
  ...PropsA,
  propB: string,
  ...
}>;
