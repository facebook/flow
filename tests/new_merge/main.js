// @flow

import { x as x1, type T as T1 } from './primitive';
(1: T1);
(x1: T1);
("": T1); // error string ~> nunber

import { x as x2 } from './intersection';
(x2: empty); // okay - this is already an error at the definition site

import { x as x3, type T as T3, type S as S3, type R as R3 } from './lookup';
(x3: number);
(x3: empty); // error number ~> empty
declare var t3: T3;
declare var s3: S3;
(t3.f: empty); // okay - this is already an error at the definition site
(s3.f: empty); // okay - this is already an error at the definition site
(0: R3); // okay

import { x as x4 } from './get_prop_instantiation';
(x4: number);
(x4: empty); // error number ~> empty

import { x as x5, y as y5 } from './get_prop_class';
(x5: number);
(x5: empty); // error number ~> empty
(y5: empty); // error function type ~> empty

import { type T as T6 } from './mk_instance_annot';
(1: T6); // error number ~> class
declare var x6: T6;
(x6: empty); // error C ~> empty

import { Foo as Foo8, C as C8 } from './mixins';
((new Foo8).x: number); // error: Qux wins
((new Foo8).y: string); // error: Bar wins
((new Foo8).z: number); // error: Qux wins
(C8: empty); // error class ~> empty

import * as x9 from './export_named_any';
(x9: empty); // error module ~> empty
(x9.f.g: empty); // okay

import C10 from './mk_instance_obj';
import type { S as S10 } from './mk_instance_obj';
(C10: empty); // error anonymous class ~> empty
declare var s10: S10;
(s10.errors: empty); // okay - error reported at definition

import B11 from './qualified_typeapp';
B11.x;

import { p as p12 } from './recursive_module';
(p12: empty); // okay - inferred as any

import A13 from './recursive_module_cycle_A';
(A13: empty); // okay - inferred as any

import { A as A14, B as B14, x as x14 } from './unification';
const y14: Array<A14 /* tvar 2 */> = x14; // unify 1(B) = 2(A), update 1 goto 2, expected error
const z14: Array<B14 /* tvar 3 */> = x14; // okay

import { f as f15 } from './async_void_return';
(f15(): empty);

import {
  r as r16,
  x as x16,
  C as C16,
  arr as arr16,
  type T as T16,
  type S as S16,
  type DisjoinUnionOpt as D16,
} from './recursive_types';
(r16: empty); // okay - inferred as any
(x16.r: empty); // okay - inferred as any

declare var t16: T16;
(1: S16); // error number ~> string
(t16: empty); // okay - inferred as any
(t16.A: empty); // okay - inferred as any
(C16: empty); // error class ~> empty
(arr16: $ReadOnlyArray<empty>); // okay - inferred as any

declare var d16: D16;
if (d16.kind === 'kind1') {
  (d16.items[0]: null);
}

import { E as E17 } from './enums';
(E17: empty); // error enum ~> empty
(E17.A: empty); // okay?
(E17.C: empty); // error - not a member

import { x as x18, y as y18, z as z18 } from './eval_readonly';
(x18: empty); // error number, string ~> empty
(y18: empty); // error number ~> empty
(z18: empty); // error string ~> empty

import { x as x19, y as y19, z as z19 } from './spread';
(x19: { f: number });
(y19: { a?: number }); // okay
(y19: { a: number }); // error undefined ~> number
(z19: empty); // okay any ~> empty (error on def site)

import C20 from './existential';
(C20: empty); // error C ~> empty

import { type T as T21 } from './mk_instance_poly';
declare var t21: T21;
(t21.c: empty); // okay - this is already an error at the definition site

import {
  p1 as p1_22,
  p2 as p2_22,
  p3 as p3_22,
  p4 as p4_22,
  p1_f as p1_f_22,
  p2_f as p2_f_22,
  p3_f as p3_f_22,
  p4_f as p4_f_22,
  p1_g as p1_g_22,
  p2_g as p2_g_22,
  p3_g as p3_g_22,
  p4_g as p4_g_22,
} from './eval_spread';

(p1_f_22: empty); // error string ~> empty
(p2_f_22: empty); // error string ~> empty
(p3_f_22: empty); // error string ~> empty
(p4_f_22: empty); // error string ~> empty
(p1_g_22: empty); // error string ~> empty
(p2_g_22: empty); // error string ~> empty
(p3_g_22: empty); // error string ~> empty
(p4_g_22: empty); // error string ~> empty

(p1_22: { f: string, g: string, ... });
(p2_22: { f: string, g: string, ... });
(p3_22: { f: string, g: string, ... });
(p4_22: { f: string, g: string, ... });

(p1_22: {| f: string, g: string |}); // error incompatible exact
(p2_22: {| f: string, g: string |}); // error incompatible exact
(p3_22: {| f: string, g: string |});
(p4_22: {| f: string, g: string |});

import {
  d_ as d23,
  r1_ as r1_23,
  r2_ as r2_23,
  r3_ as r3_23,
} from './eval_rest';

(d23: empty); // error string ~> empty
(r1_23: empty); // error string ~> empty
(r2_23: empty); // error string and undefined ~> empty
(r3_23: empty); // error string and undefined ~> empty

import type { T as T24 } from './keys';
declare var t24: T24;
(t24: empty); // error number (length) ~> empty

import { x as x25 } from './obj_rest';
(x25: empty); // error number ~> empty
