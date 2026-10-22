// Type-only exports stay available through cyclic imports and barrel re-exports.
import type { L } from './alias_b';
import type { L as ReexportedL } from './alias_a';
import type { LocaleOptions, Helper } from './interface_a';
import type { LocaleOptions as ReexportedOptions } from './barrel';
// Classes can be re-exported as types; their values and functions are not forwarded.
import { Helper as HelperValue } from './interface_a'; // ERROR
import type { fn } from './interface_a'; // ERROR

// An alias imported directly from the cycle retains its numeric type.
declare const alias: L;
alias as number; // OK
alias as string; // ERROR

// The same alias retains its numeric type through the type star re-export.
declare const reexportedAlias: ReexportedL;
reexportedAlias as number; // OK
reexportedAlias as string; // ERROR

// An interface extending a type from the cycle retains its inherited property.
declare const options: LocaleOptions;
options.weekStartsOn as number; // OK
options.weekStartsOn as string; // ERROR

// The inherited property also stays precise through an additional barrel export.
declare const reexportedOptions: ReexportedOptions;
reexportedOptions.weekStartsOn as number; // OK
reexportedOptions.weekStartsOn as string; // ERROR

// A class forwarded as a type retains the types of its instance properties.
declare const helper: Helper;
helper.value as number; // OK
helper.value as string; // ERROR
