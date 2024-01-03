// @flow

import type {PropsB} from './fileA2';

export type PropsA = {
  propA: Data,
  ...
};

export type Data = $ReadOnly<
  | { ...PropsA, ... } // TODO should not error here
  | PropsB // TODO should not error here
>;

declare var data: Data;
(data.propA: empty); // TODO error obj ~> empty
