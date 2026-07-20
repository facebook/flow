import { E } from './exported';

declare const e: E;

e.foo as 1; // OK
e.go() as number; // OK
e.ext as number; // OK: member-expression key survives the module boundary
E.foo as string; // OK

e.foo as 2; // ERROR: 1 is not 2
e.go() as string; // ERROR: number is not string
e.ext as string; // ERROR: number is not string
