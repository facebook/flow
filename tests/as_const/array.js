// Definitions

const a = [1, 2, 3];
declare const b: Array<number>;
declare const n: number;

export const arr1 = [1, 2, 3] as const;   // readonly [1, 2, 3]
export const arr2 = [n] as const;         // readonly [number]

const arr3 = [...[1, 2, 3]] as const;      // readonly [1, 2, 3]
const arr4 = [...a] as const;              // readonly [Num<1>, Num<2>, Num<3>]
const arr5 = [...b] as const;              // readonly [number, number, number]
export const arr6 = [{f: 1}] as const;            // readonly [{+f: 1}]
const arr7 = [...[1, ...[2, 3]]] as const; // readonly [1, 2, 3]
export const arr8 = [] as const;           // []

declare var _: any;

// Tests

function test_arr1() {
  arr1 as [1, 2, 3]; // error cast to non-readonly
  arr1 as readonly [1, 2, 3]; // okay
  arr1 as readonly [1, 2, 3, ...]; // okay
  arr1.push(4); // error cannot mutate readonly array

  _ as [1, 2, 3] as typeof arr1; // okay
  _ as readonly [1, 2, 3] as typeof arr1; // okay
  _ as readonly [1, 2, 3, ...] as typeof arr1; // ERROR: inexact ~> exact
  _ as [number, 1, 3] as typeof arr1; // error number ~> 1, 1 ~> 2
}

function test_arr2() {
  arr2 as [number]; // error cast to non-readonly
  arr2 as readonly [number]; // okay
}

function test_arr3() {
  arr3 as typeof arr1; // okay
  arr1 as typeof arr3; // okay
}

function test_arr4() {
  arr4 as readonly [1, 2, 3]; // error arr4 elements are `number` not `1`, `2`, `3`
  arr4 as readonly [number, number, number]; // okay

  _ as [1, 2, 3] as typeof arr4; // okay
  _ as [number, number, number] as typeof arr4; // okay
  _ as readonly [number, number, number] as typeof arr4; // okay
}

function test_arr5() {
  arr5 as [1, 2, 3]; // error arr5 is not a tuple
  arr5 as Array<number>; // error arr5 is readonly
  arr5 as $ReadOnlyArray<number>; // okay

  _ as [1, 2, 3] as typeof arr5; // okay
  _ as readonly [1, 2, 3] as typeof arr5; // okay
  _ as [number] as typeof arr5; // okay
  _ as readonly [number] as typeof arr5; // okay
}

function test_arr6() {
  arr6 as readonly [{+f: 1}]; // okay
  arr6 as [{+f: 1}]; // error cast to non-readonly array
  arr6 as [{f: 1}]; // error cast to non-readonly prop
  arr6 as readonly [{+f: number}]; // okay

  _ as [{f: 1}] as typeof arr6; // okay
  _ as readonly [{+f: 1}] as typeof arr6; // okay
}

function test_arr7() {
  arr7 as typeof arr1; // okay
  arr1 as typeof arr7; // okay
}

function test_arr8() {
  arr8 as []; // okay
  arr8 as [...]; // okay
  arr8 as [a?: ?number]; // error arity

  _ as [] as typeof arr8; // okay
  _ as [1] as typeof arr8; // error arity
  _ as [a?: number] as typeof arr8; // error arity
}
