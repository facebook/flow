// @flow

import {f1, f2, f3, f4, poly1, poly2, poly3, r1, r2, r3, r4, r5, r6, r7, r8, r9} from './function_typeof.js';

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
f4({f: [1, 2, 3]}, '1'); // error

poly1(1, 2); // TODO okay
poly1(1, 'a'); // error (TODO string ~> number)

poly2({f: 1}, 3); // okay
poly2({f: 1}, 'a'); // error string ~> number

poly3({f: 1}, 3); // TODO okay
poly3({f: 1}, 'a'); // error (TODO string ~> number)

r1 as (x: number) => number; // ok
r1 as (x: number) => string; // error

r2 as (x: {f: number}) => number; // ok
r2 as (x: {f: number}) => string; // error

r3 as (...x: Array<number>) => Array<number>; // ok

r4 as <X>(x: X) => X; // ok
r4 as <X>(x: X) => empty // TODO error

r5 as <X>(x: X) => {f: X}; // ok
r5 as <X>(x: X) => {f: empty}; // error

r6 as <X: {f: number}>(x: X) => number; // ok
r6 as <X: {f: number}>(x: X) => string; // error

r7 as <V, X: {f: V}>(x: X) => V; // ok
r7 as <V, X: {f: V}>(x: X) => empty; // error

r8 as <X>(...x: Array<X>) => Array<X>; // ok
r8 as <X>(...x: Array<X>) => Array<empty>; // TODO error

{
    const x9_1 = r9(true); // inferred as 2
    x9_1 as 1; // error 2 ~> 1
    x9_1 as 2; // okay
    const x9_2 = r9(false); // inferred as 2
    x9_2 as 1; // error 2 ~> 1
    x9_2 as 2; // okay
}
