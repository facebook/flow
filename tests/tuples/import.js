import type {T}  from './export';

[0, 1, 2, 3, 4] as T; // OK
[0, 1, 2, 3] as T; // OK
[0, 1, 2] as T; // OK

[0, 1, 9] as T; // ERROR
declare const x: T;
x as empty; // ERROR
