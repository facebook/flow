import {type T, type R, type RenamedT, value} from './test.js';
import {type S} from './test.js';

// Value export is usable as a value
value as string; // OK

// Verify types resolve to correct types
declare const t: T;
t as string; // OK
t as empty; // ERROR

declare const s: S;
s as number; // OK
s as empty; // ERROR

declare const r: R;
r as boolean; // OK
r as empty; // ERROR

declare const rt: RenamedT;
rt as string; // OK
rt as empty; // ERROR
