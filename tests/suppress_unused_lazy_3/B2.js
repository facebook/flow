// @flow

import {b1} from './B1';
import {b3} from './B3';

(b1: number);
(b3: number);

// $FlowFixMe[incompatible-type] -- used suppression in B2.js
(b1: string);

export const b2: number | number = b1;

type T = string;
// $FlowFixMe[incompatible-type] This is an unused suppression
// $FlowFixMe[incompatible-use] This is a used suppression for a misplaced error caused by B4
export type B2 = T<string>;

export {b1} // ensure B4 is checked when B1.js changes
