// @flow

import type {PropsB} from './fileB2';

export type PropsA = Readonly<{
  propA: Data,
  ...
}>;

export type Data = Readonly<
  | { ...PropsA, ... }
  | PropsB
>;

declare const data: Data;
data.propA as empty; // error obj ~> empty
