// @flow
// The imported shapes must behave exactly as the within-file ones do, so the
// signature pipeline and the checking pipeline agree on which computed method
// keys are named methods.
import {Holder} from './exp';
import type {
  One,
  Num,
  Neg,
  Mixed,
  Poly,
  OT,
  FromConst,
  FromQualified,
  FromPrimitiveKeyword,
  FromWide,
  LiteralFirst,
  ValueFirst,
  ValueLast,
  TwoValues,
  AccessorLast,
  AccessorFirst,
} from './exp';

declare const one: One;
one.m() as number; // OK
one['m']() as number; // OK
one.m() as string; // ERROR: number is not string

declare const num: Num;
num[42]() as number; // OK
num[42]() as string; // ERROR: number is not string

declare const neg: Neg;
neg[-1]() as number; // OK: a negative key survives the boundary
neg[-1]() as string; // ERROR: number is not string

declare const mixed: Mixed;
mixed.run() as number; // OK
mixed['other'] as boolean; // OK: via the indexer
mixed.run() as string; // ERROR: number is not string

declare const poly: Poly;
poly.id(1) as number; // OK
poly.id(1) as string; // ERROR: number is not string

declare const ot: OT;
ot.f() as number; // OK
ot.f() as string; // ERROR: number is not string

declare const fromConst: FromConst;
fromConst.run() as number; // OK
fromConst.run() as string; // ERROR: number is not string

declare const fromQualified: FromQualified;
fromQualified.go() as number; // OK
fromQualified.go() as string; // ERROR: number is not string

declare const fromPrimitiveKeyword: FromPrimitiveKeyword;
fromPrimitiveKeyword.prim() as number; // OK
fromPrimitiveKeyword.prim() as string; // ERROR: number is not string

declare const fromWide: FromWide;
fromWide.anything; // ERROR: the member names no one property

declare const lf: LiteralFirst;
lf.a() as 'q'; // OK: the literal key is written first
lf.a() as 'p'; // ERROR: 'q' is not 'p'

declare const vf: ValueFirst;
vf.a() as 'p'; // OK: the value name goes last however early it is written
vf.a() as 'q'; // ERROR: 'p' is not 'q'

declare const vl: ValueLast;
vl.a() as 'p'; // OK: and it is still last in this order
vl.a() as 'q'; // ERROR: 'p' is not 'q'

declare const tv: TwoValues;
tv.a() as 'z'; // OK: the later key is the earlier overload
tv.a() as 'q'; // ERROR: 'z' is not 'q'

declare const al: AccessorLast;
al.a as 'p'; // OK: the accessor takes the name
al.a as 'q'; // ERROR: 'p' is not 'q'

declare const af: AccessorFirst;
af.a() as 'q'; // OK: nothing is written after the key, so the method wins
af.a() as 'p'; // ERROR: the accessor lost the name, so 'q' is not 'p'

declare const holder: Holder;
holder.dm() as number; // OK
Holder.sm() as string; // OK
holder[-1]() as boolean; // OK
holder.dm() as string; // ERROR: number is not string
