// Object destructuring with shorthand properties
export declare function f({a, b}: {a: number, b: string}): number;
// Object destructuring with property renaming
export declare function g({a: b}: {a: number}): number;
// Array destructuring
export declare function h([a, b]: [number, string]): number;
// Nested destructuring
export declare function nested({a: {b}}: {a: {b: number}}): number;
// Rest element in destructuring
export declare function rest({a, ...b}: {a: number, c: string, d: boolean}): number;
// Optional object destructuring
export declare function optObj({a}?: {a: number}): number;
// Optional array destructuring
export declare function optArr([a]?: [number]): number;
// Declare class with destructuring in method
export declare class C {
  m({a, b}: {a: number, b: string}): number;
  n([a]: [number]): string;
  o({a}?: {a: number}): number;
}
// Interface with destructuring in method
export interface I {
  m({a, b}: {a: number, b: string}): number;
  n([a]: [number]): string;
}
// Rest param with array destructuring (valid TS pattern)
export declare function restArr(...[a, b]: [number, string]): number;
// Arrow function types with destructuring
export type ArrowDestructObj = ({a, b}: {a: number, b: string}) => number;
export type ArrowDestructArr = ([a, b]: [number, string]) => number;
export type ArrowDestructRename = ({x: x1}: {x: number}) => number;
