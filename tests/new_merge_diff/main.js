// @flow

import { x as x1 } from './get_prop_union';
// old merge error: number ~> empty
// new merge error: number | string ~> empty
(x1: empty);

import typeof T3 from './export_error';
// in new-merge, we no longer error here as we already error in export_error.js
(0: T3);

import { p as p12 } from './recursive_module';
(p12: empty); // okay - inferred as any

import A13 from './recursive_module_cycle_A';
(A13: empty); // okay - inferred as any

import {
  x as x4,
  y as y4,
  z as z4,
  f as f4,
  type T as T4,
  type S as S4,
  type R as R4,
} from './recursive_types';

(x4: any);
(y4: any);
(x4: any);
(f4: any);

(0: T4);
(0: S4); // error number ~> string
(0: R4); // error number ~> R (obj)
