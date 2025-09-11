// @flow

import {f1, f2, f3, f4, poly1, poly2, poly3} from './function_typeof.js';

f1 as (x: mixed) => void; // ok because f1 is (x: any) => void
f2 as (x: number, y: number) => void; // ok
f3 as (x: {f: number}, y: number) => void; // ok
f4 as (x: {f: Array<number>}, ...y: Array<number>) => void; // ok

poly1 as <X>(x: X, y: X) => void; // ok
poly2 as <X: {f: number}>(x: X, y: number) => void; // ok
poly2 as <X: {f: number}>(x: X, y: X['f']) => void; // ok
poly3 as <V, X: {f: V}>(x: X, y: V) => void
poly3 as <V, X: {f: V}>(x: X, y: X['f']) => void;

f1(1); // okay
f1('a'); // okay

f2(1, 2); // okay
f2(1, 'a'); // error

f3({f: 1}, 2); // okay
f3({f: 1}, 'a'); // error

f4({f: [1, 2, 3]}, 4, 5, 6);
f4({f: [1, 2, 3]}, '1'); // TODO error

poly1(1, 2); // TODO okay
poly1(1, 'a'); // error (TODO string ~> number)

poly2({f: 1}, 3); // okay
poly2({f: 1}, 'a'); // error string ~> number

poly3({f: 1}, 3); // TODO okay
poly3({f: 1}, 'a'); // error (TODO string ~> number)
