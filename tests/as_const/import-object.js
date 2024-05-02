// Imports

import {obj1, obj2, obj3, obj4, obj5, obj6, obj7, obj8, obj9} from './object';

declare var _: any;

// Tests

function test_obj1() {
  obj1 as {f: 1}; // error cast to non-readonly
  obj1 as {+f: 1}; // okay
  obj1 as {+f: 1, ...}; // okay
  obj1 as {+f: number}; // okay

  _ as {f: 1} as typeof obj1; // okay
  _ as {+f: 1} as typeof obj1; // okay
  _ as {+f: 1, ...} as typeof obj1; // error inexact ~> exact
  _ as {f: number} as typeof obj1; // error number ~> 1
}

function test_obj2() {
  obj2 as {+p: {f: 1}, +q: {f: number}, +d: {[string]: number}};
  _ as {
    +p: {f: number},
    +q: {f: number},
    +d: {[string]: number},
  } as typeof obj2;
}

function test_obj3() {
  obj3 as {f: 1}; // error cast to non-readonly
  obj3 as {+f: 1}; // okay
  obj3 as {+f: 1, ...}; // okay
  obj3 as {+f: number}; // okay

  _ as {f: 1} as typeof obj3; // okay
  _ as {+f: 1} as typeof obj3; // okay
  _ as {+f: 1, ...} as typeof obj3; // error inexact ~> exact
  _ as {f: number} as typeof obj3; // error number ~> 1
}

function test_obj4() {
  obj4 as {f: 1}; // error cast to non-readonly
  obj4 as {+f: 1}; // okay
  obj4 as {+f: 1, ...}; // okay
  obj4 as {+f: number}; // okay

  _ as {f: 1} as typeof obj4; // okay
  _ as {+f: 1} as typeof obj4; // okay
  _ as {+f: 1, ...} as typeof obj4; // error inexact ~> exact
  _ as {f: number} as typeof obj4; // okay number ~> Num<1>
}

function test_obj5() {
  obj5 as {f: number}; // error cast to non-readonly
  obj5 as {+f: number}; // okay
  obj5 as {+f: 1, ...}; // error number ~> 1

  _ as {f: number} as typeof obj5; // okay
  _ as {+f: number} as typeof obj5; // okay
  _ as {+f: number, ...} as typeof obj5; // error inexact ~> exact
}

function test_obj6() {
  obj6 as {[string]: number}; // error readonly ~> non-readonly
  obj6 as {+[string]: number}; // okay

  _ as {[string]: number} as typeof obj6; // okay
  _ as {+[string]: number} as typeof obj6; // okay
}

function test_obj7() {
  obj7 as {m(): void}; // okay m is readonly
  obj7 as {m: () => void}; // error m is readonly in obj7
  obj7 as {+m: () => void}; // okay

  _ as {m(): void} as typeof obj7; // okay
  _ as {+m: () => void} as typeof obj7; // okay
  _ as {m: () => void} as typeof obj7; // okay
}

function test_obj8() {
  obj8 as {+f: {g: 1}}; // error cast to non-readonly g
  obj8 as {+f: {+g: 1}}; // okay
  obj8 as {+f: {+g: 1, ...}}; // okay
  obj8 as {+f: {+g: number}}; // okay

  _ as {f: {g: 1}} as typeof obj8; // okay
  _ as {+f: {g: 1}} as typeof obj8; // okay
  _ as {+f: {+g: 1}} as typeof obj8; // okay
  _ as {+f: {+g: 1, ...}} as typeof obj8; // error inexact ~> exact
  _ as {f: {g: number}} as typeof obj8; // error number ~> 1
}

function test_obj9() {
  obj9 as {+f: {+g: {+h: 1}}}; // okay

  _ as {+f: {+g: {+h: 1}}} as typeof obj9; // okay
}
