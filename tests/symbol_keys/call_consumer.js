import {C, annotated, registered, table} from './call_exported';

// Each of the four symbols survives the module boundary and keys the member it
// keys in the file that declares it.
table[annotated] as number; // OK
table[registered] as string; // OK
table[C.K] as boolean; // OK
table[C.L] as null; // OK

table[annotated] as string; // ERROR: number is not string

// A symbol introduced here is a different symbol, whichever of the two forms
// introduces it.
const local = Symbol();
table[local]; // ERROR: not a key of `table`

const localAnnotated: unique symbol = Symbol();
table[localAnnotated]; // ERROR: not a key of `table`
