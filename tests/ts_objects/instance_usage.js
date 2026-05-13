// @flow
//
// Cross-file: a `.js` consumer of types declared in `instance_lib.ts`.
// Class and interface instances flow into both exact (default in `.js` with
// `exact_by_default`) and explicit-inexact `{..., ...}` object targets.
//
// The Gate-keyed relaxations (suppress `EClassToObject` for `InstanceT ->
// ObjT`, suppress incompatibility for `InstanceT -> Exact ObjT`) are scoped
// to `.ts` consumers, so a `.js` consumer must still see the original
// strict diagnostics. This pins the consumer-keyed scoping.

import {Box} from './instance_lib';
import type {IBox} from './instance_lib';

declare const b: Box;
declare const ib: IBox;

// Class instance into exact target (`{}` is exact by default in `.js`):
// errors in `.js` consumer.
b as {v: number}; // ERROR

// Class instance into explicit-inexact target: still errors in `.js`
// consumer because the EClassToObject relaxation is also consumer-keyed.
b as {v: number, ...}; // ERROR

// Interface instance into exact target: errors in `.js` consumer.
ib as {v: number}; // ERROR

// Interface instance into explicit-inexact target: also errors -- the
// EClassToObject suppression is consumer-keyed and applies to InterfaceKind
// too.
ib as {v: number, ...}; // ERROR
