// @flow

import {x as x1, type T as T1} from './primitive';
1 as T1;
x1 as T1;
'' as T1; // error string ~> nunber

import {x as x2} from './intersection';
x2 as empty; // okay - this is already an error at the definition site

import {x as x3, type T as T3, type S as S3, type R as R3} from './lookup';
x3 as number;
x3 as empty; // error number ~> empty
declare var t3: T3;
declare var s3: S3;
t3.f as empty; // okay - this is already an error at the definition site
s3.f as empty; // okay - this is already an error at the definition site
0 as R3; // okay

import {x as x4} from './get_prop_instantiation';
x4 as number;
x4 as empty; // error number ~> empty

import {x as x5, y as y5} from './get_prop_class';
x5 as number;
x5 as empty; // error number ~> empty
y5 as empty; // error function type ~> empty

import {type T as T6} from './mk_instance_annot';
1 as T6; // error number ~> class
declare var x6: T6;
x6 as empty; // error C ~> empty

import {Foo as Foo8, C as C8} from './mixins';
new Foo8().x as number; // error: Qux wins
new Foo8().y as string; // error: Bar wins
new Foo8().z as number; // error: Qux wins
C8 as empty; // error class ~> empty

import * as x9 from './export_named_any';
x9 as empty; // error module ~> empty
x9.f.g as empty; // okay

import C10 from './mk_instance_obj';
import type {S as S10} from './mk_instance_obj';
C10 as empty; // error anonymous class ~> empty
declare var s10: S10;
s10.errors as empty; // okay - error reported at definition

import B11 from './qualified_typeapp';
B11.x;

import {A as A14, B as B14, x as x14} from './unification';
const y14: Array<A14 /* tvar 2 */> = x14; // unify 1(B) = 2(A), update 1 goto 2, expected error
const z14: Array<B14 /* tvar 3 */> = x14; // okay

import {f as f15} from './async_void_return';
f15() as empty;

declare var t16: T16;
1 as S16; // error number ~> string
t16 as empty; // okay - inferred as any
t16.A as empty; // okay - inferred as any
C16 as empty; // error class ~> empty
arr16 as $ReadOnlyArray<empty>; // okay - inferred as any

declare var d16: D16;
if (d16.kind === 'kind1') {
  d16.items[0] as null;
}

import {E as E17} from './enums';
E17 as empty; // error enum ~> empty
E17.A as empty; // error enum ~> empty
E17.C as empty; // error - not a member

import {x as x18, y as y18, z as z18} from './eval_readonly';
x18 as empty; // error number, string ~> empty
y18 as empty; // error number ~> empty
z18 as empty; // error string ~> empty

import {baz1 as baz01, baz2 as baz02} from './eval_indexed_access';
baz01 as empty; // error number ~> empty
baz02 as empty; // error number ~> empty

import {x as x19, y as y19, z as z19} from './spread';
x19 as {f: number};
y19 as {a?: number}; // okay
y19 as {a: number}; // error undefined ~> number
z19 as empty; // okay any ~> empty (error on def site)

import {type T as T21} from './mk_instance_poly';
declare var t21: T21;
t21.c as empty; // okay - this is already an error at the definition site

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

p1_f_22 as empty; // error string ~> empty
p2_f_22 as empty; // error string ~> empty
p3_f_22 as empty; // error string ~> empty
p4_f_22 as empty; // error string ~> empty
p1_g_22 as empty; // error string ~> empty
p2_g_22 as empty; // error string ~> empty
p3_g_22 as empty; // error string ~> empty
p4_g_22 as empty; // error string ~> empty

p1_22 as {f: string, g: string, ...};
p2_22 as {f: string, g: string, ...};
p3_22 as {f: string, g: string, ...};
p4_22 as {f: string, g: string, ...};

p1_22 as {|f: string, g: string|}; // error incompatible exact
p2_22 as {|f: string, g: string|}; // error incompatible exact
p3_22 as {|f: string, g: string|};
p4_22 as {|f: string, g: string|};

import {d_ as d23, r1_ as r1_23, r2_ as r2_23, r3_ as r3_23} from './eval_rest';

d23 as empty; // error string ~> empty
r1_23 as empty; // error string ~> empty
r2_23 as empty; // error string and undefined ~> empty
r3_23 as empty; // error string and undefined ~> empty

import type {T as T24} from './keys';
declare var t24: T24;
t24 as empty; // error number (length) ~> empty

import {x as x25} from './obj_rest';
x25 as empty; // error number ~> empty

import type {IndirectFrozenSuiteValues} from './frozen_obj';

type FrozenSuiteValues =
  | 'Diamonds'
  | 'Clubs'
  | 'Hearts'
  | 'Spades'
  | 'Extra Suite';

declare var frozenSuitevalues: FrozenSuiteValues;
frozenSuitevalues as IndirectFrozenSuiteValues; // okay - due to OpenT indirection

declare var indirectFrozenSuitevalues: IndirectFrozenSuiteValues;
indirectFrozenSuitevalues as FrozenSuiteValues; // okay

import S26 from './type_of_typeapp';
S26.getState() as empty; // error RecordInstance ~> empty

import {f as f27, C as C27, x as x27} from './recursive';

f27() as any;
C27.Q as number;
C27.Q as string; // error
x27.q as number;
x27.q as string; // error

import type {T as T28} from './values';
declare var t28: T28;
t28 as empty; // error number (length) ~> empty

import {instance_named, instance_computed} from './get_prop_instance';
instance_named as number; // OK
instance_named as empty; // ERROR
instance_computed as empty; // OK (export is error)

import {x as t29} from './union';
t29 as empty; // okay - this is already an error at the definition site
