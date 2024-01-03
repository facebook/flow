// @flow

import type {PropsB} from './fileA2';

export type PropsA = {
  propA: Data,
  ...
};

export type Data = $ReadOnly<
  | { ...PropsA, ... }
  | PropsB
>;

declare var data: Data;
(data.propA: empty); // error obj ~> empty
