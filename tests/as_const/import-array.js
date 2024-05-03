import {arr1, arr2, arr6, arr8} from './array';

declare var _: any;

// Tests

function test_arr1() {
  arr1 as [+_:1, +_:2, 3]; // error cast 3rd element to non-readonly
  arr1 as readonly [1, 2, 3]; // okay
  arr1 as readonly [1, 2, 3, ...]; // okay

  _ as [1, 2, 3] as typeof arr1; // okay
  _ as readonly [1, 2, 3] as typeof arr1; // okay
  _ as readonly [1, 2, 3, ...] as typeof arr1; // TODO error inexact ~> exact
  _ as [number, 1, 3] as typeof arr1; // error number ~> 1, 1 ~> 2
}

function test_arr2() {
  arr2 as [number]; // error cast to non-readonly
  arr2 as readonly [number]; // okay
}


function test_arr6() {
  arr6 as readonly [{+f: 1}]; // okay
  arr6 as [{+f: 1}]; // error cast to non-readonly array
  arr6 as readonly [{f: 1}]; // error cast to non-readonly prop
  arr6 as readonly [{+f: number}]; // okay

  _ as [{f: 1}] as typeof arr6; // okay
  _ as readonly [{+f: 1}] as typeof arr6; // okay
}

function test_arr8() {
  arr8 as []; // okay
  arr8 as [...]; // okay
  arr8 as [a?: ?number]; // error arity

  _ as [] as typeof arr8; // okay
  _ as [1] as typeof arr8; // error arity
  _ as [a?: number] as typeof arr8; // error arity
}
