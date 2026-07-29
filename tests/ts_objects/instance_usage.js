// @flow
//
// Cross-file: a `.js` consumer of types declared in `instance_lib.ts`.
// Class and interface instances flow into both exact (default in `.js` with
// `exact_by_default`) and explicit-inexact `{..., ...}` object targets.
//
// The imported class and interface retain TypeScript's structural object
// compatibility in this Flow consumer.

import {Box, Point, pointShape} from './instance_lib';
import type {IBox} from './instance_lib';

declare const b: Box;
declare const ib: IBox;

// Class instance into exact target (`{}` is exact by default in `.js`).
b as {v: number}; // OK

// Class instance into explicit-inexact target.
b as {v: number, ...}; // OK

// Interface instance into exact target.
ib as {v: number}; // OK

// Interface instance into explicit-inexact target.
ib as {v: number, ...}; // OK

pointShape as Point; // OK: both object and class come from .ts
