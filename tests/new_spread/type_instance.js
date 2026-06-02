class A {readonly p: string|number}
class B extends A {p: number}

type O1 = {...B, ...};
declare const o1: O1;
o1 as {p?:number, ...}; // Error

declare class C {[string]:number}
type O2 = {...C, ...};
declare const o2: O2;
o2 as {[string]:number}; // ok
