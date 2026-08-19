// A `unique symbol` key renders TypeScript-style as `[name]` (not the generic
// `[symbol]`) when the symbol's declaration names it: a named object property, a
// class field, or an interface field. Each is exercised both within a file (the
// name-hint path) and across a module boundary (the signature-merge path).

import {Keys, D} from './syms';
import type {I} from './syms';

// Object property, within-file: the value type of property `a` is a `unique
// symbol` named `a`.
declare const local: {readonly a: unique symbol};
const obj1 = {[local.a]: 1};
//    ^
// type-at-pos should print the key as `[a]`.

// Object property, cross-module: `syms.d.ts` names the symbol `a` during
// signature merge.
const obj2 = {[Keys.a]: 2};
//    ^
// type-at-pos should print the key as `[a]`.

// Class field, within-file: the static field `sk` is a `unique symbol` named
// after the field. Only the static side can spell one out, so a class
// contributes no instance case here; the interface fields below cover a symbol
// reached through an instance value.
declare class C {
  static readonly sk: unique symbol;
}
const obj3 = {[C.sk]: 3};
//    ^
// type-at-pos should print the key as `[sk]`.

// A value-level `class` names its static field the same way, though it reaches
// the symbol by a different path than the `declare class` above.
class VC {
  static readonly vsk: unique symbol;
}
const obj4 = {[VC.vsk]: 4};
//    ^
// type-at-pos should print the key as `[vsk]`.

// Class field, cross-module: `syms.d.ts` names the static field `dsk` during
// signature merge.
const obj5 = {[D.dsk]: 5};
//    ^
// type-at-pos should print the key as `[dsk]`.

// Interface field, within-file: the field `jfk` is a `unique symbol` named `jfk`.
interface J {
  readonly jfk: unique symbol;
}
declare const j: J;
const obj7 = {[j.jfk]: 7};
//    ^
// type-at-pos should print the key as `[jfk]`.

// Interface field, cross-module: `syms.d.ts` names the field `ifk` during
// signature merge.
declare const i: I;
const obj8 = {[i.ifk]: 8};
//    ^
// type-at-pos should print the key as `[ifk]`.
