import type {T}  from './export';

([0, 1, 2, 3, 4]: T); // OK
([0, 1, 2, 3]: T); // OK
([0, 1, 2]: T); // OK

([0, 1, 9]: T); // ERROR
declare const x: T;
(x: empty); // ERROR
