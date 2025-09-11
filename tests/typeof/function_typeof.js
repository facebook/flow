/**
 * @flow
 */

export function f1(x: typeof x) {} // error - unbound x
export function f2(x: number, y: typeof x) {}
export function f3(x: {f: number}, y: typeof x.f) {}
export function f4(x: {f: Array<number>}, ...y: typeof x.f) {}

export function poly1<X>(x: X, y: typeof x) {}
export function poly2<X: {f: number}>(x: X, y: typeof x.f) {}
export function poly3<V, X: {f: V}>(x: X, y: typeof x.f) {}

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

export function r1(x: number): typeof x { return x; }
export function r2(x: {f: number}): typeof x.f { return x.f; }
export function r3(...x: Array<number>): typeof x { return x; }
export function r4<X>(x: X): typeof x { return x; }
export function r5<X>(x: X): {f:typeof x} { return {f: x}; }
export function r6<X: {f: number}>(x: X): typeof x.f { return x.f; }
export function r7<V, X: {f: V}>(x: X): typeof x.f { return x.f; }
export function r8<X>(...x: Array<X>): typeof x { return x; }
export function r9(b: boolean): typeof b extends true ? 1 : 2 { return 2; }

r1 as (x: number) => number; // ok
r1 as (x: number) => string; // error

r2 as (x: {f: number}) => number; // ok
r2 as (x: {f: number}) => string; // error

r3 as (...x: Array<number>) => Array<number>; // ok

r4 as <X>(x: X) => X; // ok
r4 as <X>(x: X) => empty // error

r5 as <X>(x: X) => {f: X}; // ok
r5 as <X>(x: X) => {f: empty}; // error

r6 as <X: {f: number}>(x: X) => number; // ok
r6 as <X: {f: number}>(x: X) => string; // error

r7 as <V, X: {f: V}>(x: X) => V; // ok
r7 as <V, X: {f: V}>(x: X) => empty; // error

r8 as <X>(...x: Array<X>) => Array<X>; // ok
r8 as <X>(...x: Array<X>) => Array<empty>; // error

{
    const x9_1 = r9(true); // inferred as 2
    x9_1 as 1; // error 2 ~> 1
    x9_1 as 2; // okay
    const x9_2 = r9(false); // inferred as 2
    x9_2 as 1; // error 2 ~> 1
    x9_2 as 2; // okay
}
