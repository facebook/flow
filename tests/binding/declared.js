function var_redeclaration_test_1() {
  declare var x: number;
  declare var x: string; // error
  (x: number); // ok
  (x: string); // error: should read the first x
}
function var_redeclaration_test_2() {
  declare var x: number;
  var x: string = ''; // error
  (x: number); // ok
  (x: string); // error: should read the first x
}
function var_redeclaration_test_3() {
  var x: number = 1;
  declare var x: string; // error
  (x: empty); // ok: read of illegal write
}

function let_redeclaration_test_1() {
  declare let x: number;
  declare let x: string; // error
  (x: number); // ok
  (x: string); // error: should read the first x
}
function let_redeclaration_test_2() {
  declare let x: number;
  let x: string = ''; // error
  (x: number); // ok
  (x: string); // error: should read the first x
}
function let_redeclaration_test_3() {
  let x: number = 1;
  declare let x: string; // error
  (x: number); // ok
  (x: string); // error: should read the first x
}

function const_redeclaration_test_1() {
  declare const x: number;
  declare const x: string; // error
  (x: number); // ok
  (x: string); // error: should read the first x
}
function const_redeclaration_test_2() {
  declare const x: number;
  const x: string = ''; // error
  (x: number); // ok
  (x: string); // error: should read the first x
}
function const_redeclaration_test_3() {
  const x: number = 1;
  declare const x: string; // error
  (x: number); // ok
  (x: string); // error: should read the first x
}

function class_redeclaration_test_1() {
  declare class A {}
  declare class B {}
  declare class C extends A {}
  declare class C extends B {}
  (new C: A); // ok
  (new C: B); // error: should read the first C
}
function class_redeclaration_test_2() {
  declare class A {}
  declare class B {}
  declare class C extends A {}
  class C extends B {}
  (new C: A); // ok
  (new C: B); // error: should read the first C
}
function class_redeclaration_test_3() {
  declare class A {}
  declare class B {}
  class C extends A {}
  declare class C extends B {}
  (new C: A); // ok
  (new C: B); // error: should read the first C
}

function var_forward_ref_test() {
  (x: number); // ok
  declare var x: number;
}

function let_forward_ref_test() {
  (x: number); // ok
  declare let x: number;
}

function const_forward_ref_test() {
  (x: number); // ok
  declare const x: number;
}

function class_forward_ref_test() {
  (new A: A); // ok
  declare class A {}
}
