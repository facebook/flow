import { x as x1 } from './get_prop_union';
// old merge error: number ~> empty
// new merge error: number | string ~> empty
x1 as empty;

import typeof T3 from './export_error';
// in new-merge, we no longer error here as we already error in export_error.js
0 as T3;

import { p as p12 } from './recursive_module';
p12 as empty; // okay - inferred as any

import A13 from './recursive_module_cycle_A';
A13 as empty; // okay - inferred as any

import {
  x as x4,
  y as y4,
  z as z4,
  C as C4,
  type T as T4,
  type S as S4,
  type R as R4,
} from './recursive_types';

x4 as any;
y4 as any;
x4 as any;
C4.R as any;

0 as T4;
0 as S4; // error number ~> string
0 as R4; // error number ~> R (obj)
