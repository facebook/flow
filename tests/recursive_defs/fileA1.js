// @flow

import type {PropsB} from './fileA2';

export type PropsA = {
  propA: Data,
  ...
};

export type Data = Readonly<
  | { ...PropsA, ... }
  | PropsB
>;

declare var data: Data;
data.propA as empty; // error obj ~> empty
