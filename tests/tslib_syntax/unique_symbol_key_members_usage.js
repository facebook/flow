// @flow
import {Keys} from './unique_symbol_key_members';
import type {I, WithOverload, C} from './unique_symbol_key_members';

// Interface: distinct symbol members resolve to their own distinct types.
declare const i: I;
i[Keys.a] as number; // OK
i[Keys.b] as string; // OK
i[Keys.a] as string; // ERROR: number is incompatible with string
i[Keys.b] as number; // ERROR: string is incompatible with number

// Overloaded method on one symbol key does not collapse into the other key.
declare const w: WithOverload;
w[Keys.a](1) as number; // OK
w[Keys.a]("s") as string; // OK
w[Keys.b] as boolean; // OK
w[Keys.b] as number; // ERROR: boolean is incompatible with number (b did not merge with a)

// Declare class: distinct symbol members resolve to their own distinct types.
declare const c: C;
c[Keys.a] as number; // OK
c[Keys.b] as string; // OK
c[Keys.b] as number; // ERROR: string is incompatible with number

// NOTE: symbol members resolve here because `I`, `WithOverload`, and `C` come
// from another module, so their shape is built by the signature-merge pipeline.
// An interface or `declare class` written in the file being checked is built by
// the (still string-keyed) class Signature instead, so its symbol members do not
// resolve yet. That, and value-level `class C { [Keys.a]: T }`, are handled in
// the next commit.
