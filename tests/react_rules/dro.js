declare const a: $ReactDeepReadOnly<{x: {y: number}}>;
a.x.y = 42; // error
a.x = 42; // error

declare const x: $ReactDeepReadOnly<{x: {a: number}, y: Array<{w: {v: number}}>}>;
const y = x.y;
const z = y[0]
const w = z.w;
const v = w.v;
(v: empty); //error
w.v = 0; // error
x.x.a = 42; // error
x.x.y = []; // error
x.x = (42: any);

declare const rr: $ReactDeepReadOnly<{current: number}>;
rr.current = 42; // ok

declare const w3: $ReactDeepReadOnly<{a: {b: number}, c: number}>;
const w2 = {...w3};
w2.c = 42; // ok
w2.a.b = 42; // error

declare const v3: $ReactDeepReadOnly<Array<{a: number}>>;
const v2 = [...v3];
v2[0] = {a: 42}; // ok
v2[1].a = 42; //error

import * as React from 'react';

component Foo1(x: { y: number}) {
    x.y = 42 // error;
    return null;
}
component Foo2(...rest: {bar: number, baz: Array<string>, qux:[number, Array<string>]}) {
    rest.bar = 42; // error;
    rest.baz.push("hello"); // error
    rest.qux[0] = 42; // error
    rest.qux[1].pop(); // error
    return null;
}

component A(x: {bar: number}) { return <A x={x} />; }; // ok

export type Union = 'A' | 'B';

component UnionToElemTShortcut(
    key: Union,
    data: {
      [key: Union]: {
        prop?: number,
      },
    },
  ) {
    const obj = data[key];
    obj.prop = 0; // error
    return null;
}

declare const droset: $ReactDeepReadOnly<Set<Set<number>>>;
droset.add(new Set()); // error
droset.add((42: any)); // error
droset.forEach(x => x.add(42)) // error

droset as Set<Set<number>> as typeof droset // fine

declare const dromap: $ReactDeepReadOnly<Map<{x: number}, Map<{y: number}, number>>>;
dromap.set({x: 42}, new Map()); // error
dromap.set({x: 42}, (42: any)); // error
dromap.forEach((val, key) => {
  key.x = 42; // error
  val.set({y: 420}, 42) // error
});

class CoolClass {
  prop: {x: number};
}

declare const drc: $ReactDeepReadOnly<CoolClass>;
drc.prop.x = 42 // error;
drc.prop = {x: 42}; // error;

declare const droarr: $ReactDeepReadOnly<Array<Array<number>>>;
droarr.push([]); // error
droarr[0].push(42); // error
droarr.at(0)?.push(42) // error;
