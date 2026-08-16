// `keyof` over an interface or `declare class` with `unique symbol` members
// includes those symbol keys, and a matching `unique symbol` satisfies the key
// set while a distinct one is rejected.
import {Keys} from './keys';
import type {I, C} from './keys';

Keys.a as keyof I; // OK
Keys.b as keyof I; // OK
Keys.a as keyof C; // OK

declare const other: unique symbol;
other as keyof I; // ERROR: `other` is not a key of `I`
