// @flow

import { x as x1 } from './get_prop_union';
// old merge error: number ~> empty
// new merge error: number | string ~> empty
(x1: empty);

import typeof T3 from './export_error';
// in new-merge, we no longer error here as we already error in export_error.js
(0: T3);

import type { IndirectFrozenSuiteValues } from './frozen_obj';

type FrozenSuiteValues =
  | 'Diamonds'
  | 'Clubs'
  | 'Hearts'
  | 'Spades'
  | 'Extra Suite';

declare var frozenSuitevalues: FrozenSuiteValues;
(frozenSuitevalues: IndirectFrozenSuiteValues); // error due to no OpenT indirection

declare var indirectFrozenSuitevalues: IndirectFrozenSuiteValues;
(indirectFrozenSuitevalues: FrozenSuiteValues); // okay
