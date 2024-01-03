// @flow

import type {PropsB} from './fileB2';

export type PropsA = $ReadOnly<{
  propA: Data,
  ...
}>;

export type Data = $ReadOnly<
  | { ...PropsA, ... }
  | PropsB
>;

declare var data: Data;
(data.propA: empty); // error obj ~> empty
