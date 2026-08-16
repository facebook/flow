import {s, t} from './keys';
import type {O} from './value_exported';

declare const o: O;

// The symbol-keyed members survive the module boundary and keep their identity.
o[s] as number; // OK
o[t] as string; // OK
o[s] as string; // ERROR: number is not string
