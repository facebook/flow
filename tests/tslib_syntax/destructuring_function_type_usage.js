import {f, g, h, nested, rest, optObj, optArr, C, restArr} from "./destructuring_function_type";
import type {I, ArrowDestructObj, ArrowDestructArr, ArrowDestructRename} from "./destructuring_function_type";

// Object destructuring with shorthand properties
f({a: 1, b: "hello"}) as number; // OK
f({a: 1, b: "hello"}) as string; // ERROR

// Object destructuring with property renaming
g({a: 1}) as number; // OK
g({a: 1}) as string; // ERROR

// Array destructuring
h([1, "hello"]) as number; // OK
h([1, "hello"]) as string; // ERROR

// Nested destructuring
nested({a: {b: 1}}) as number; // OK
nested({a: {b: 1}}) as string; // ERROR

// Rest element in destructuring
rest({a: 1, c: "hello", d: true}) as number; // OK
rest({a: 1, c: "hello", d: true}) as string; // ERROR

// Optional object destructuring
optObj({a: 1}) as number; // OK
optObj() as number; // OK
optObj({a: 1}) as string; // ERROR

// Optional array destructuring
optArr([1]) as number; // OK
optArr() as number; // OK
optArr([1]) as string; // ERROR

// Declare class with destructuring in method
declare const c: C;
c.m({a: 1, b: "hello"}) as number; // OK
c.m({a: 1, b: "hello"}) as string; // ERROR
c.n([1]) as string; // OK
c.n([1]) as number; // ERROR
c.o({a: 1}) as number; // OK
c.o() as number; // OK
c.o({a: 1}) as string; // ERROR

// Interface with destructuring in method
declare const i: I;
i.m({a: 1, b: "hello"}) as number; // OK
i.m({a: 1, b: "hello"}) as string; // ERROR
i.n([1]) as string; // OK
i.n([1]) as number; // ERROR

// Rest param with array destructuring
restArr(1, "hello") as number; // OK
restArr(1, "hello") as string; // ERROR

// Arrow function types with destructuring
declare const arrowObj: ArrowDestructObj;
arrowObj({a: 1, b: "hello"}) as number; // OK
arrowObj({a: 1, b: "hello"}) as string; // ERROR

declare const arrowArr: ArrowDestructArr;
arrowArr([1, "hello"]) as number; // OK
arrowArr([1, "hello"]) as string; // ERROR

declare const arrowRename: ArrowDestructRename;
arrowRename({x: 1}) as number; // OK
arrowRename({x: 1}) as string; // ERROR
