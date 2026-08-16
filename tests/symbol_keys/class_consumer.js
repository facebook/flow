import {E} from './class_exported';
import {key1, key2} from './keys';

declare const e: E;

// Symbol-keyed members survive the module boundary and keep their identity.
e[key1] as number; // OK
e[key2]() as string; // OK

e[key1] as string; // ERROR: number is incompatible with string
