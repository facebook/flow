class A {static p: number}

type O1 = {...Class<A>, ...};
declare const o1: O1;
o1 as {readonly p?:number, ...}; // ok
