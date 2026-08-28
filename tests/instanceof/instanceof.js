/* @flow */

// x instancof t
class X1 {
  foo: number;
}
class X2 {
  foo: string;
}

function x(b: boolean) {
  return b ? new X1() : new X2();
}

function consumer1(b: boolean) {
  var g = x(b);
  if (g instanceof X2) g.foo = '1337';
  else g.foo = 1337;
}

function consumer2(b: boolean) {
  var g = x(b);
  if (g instanceof X1) g.foo = '1337'; // oops
}

// x.y instanceof t
class Y1 {
  bar: X1;
}
class Y2 {
  bar: X2;
}

function y(b: boolean) {
  return b ? new Y1() : new Y2();
}

function consumer3(b: boolean) {
  var g = y(b);
  if (g.bar instanceof X2) g.bar.foo = '1337';
  else g.bar.foo = 1337;
}

function consumer4(b: boolean) {
  var g = y(b);
  if (g.bar instanceof X1) g.bar.foo = '1337'; // oops
}

// x.y.z instance of t
class Z1 {
  baz: Y1;
}
class Z2 {
  baz: Y2;
}

function z(b: boolean) {
  return b ? new Z1() : new Z2();
}

function consumer5(b: boolean) {
  var g = z(b);
  if (g.baz.bar instanceof X2) g.baz.bar.foo = '1337';
  else g.baz.bar.foo = 1337;
}

function consumer6(b: boolean) {
  var g = z(b);
  if (g.baz.bar instanceof X1) g.baz.bar.foo = '1337'; // oops
}

// this instanceof t
class C {
  m() {
    if (this instanceof D) console.log(this.s);
    else console.log('nope');
  }
}

class D extends C {
  s: string;
  constructor() {
    super();
    this.s = 'yup';
  }
}

function foo0(x: Array<number> | number) {
  if (x instanceof Array) {
    x[0] = 123;
  } else {
    x++;
  }
}

function foo1(x: Array<number> | number) {
  if (x instanceof Array) {
    x++; // error
  } else {
    x[0] = 123; // error
  }
}

function nonObjectRHS(x: any) {
  const y = x instanceof 'bad'; // error
  if (x instanceof 'bad') {
    x;
  } // error
  if (x instanceof ('bad' as any)) {
    x;
  } // ok
  if (x instanceof ('bad' as unknown)) {
    x;
  } // error
}

function not_refinement_or_val_rhs(x: any) {
  const immutable = {Map: class Map {}};
  if (x instanceof immutable.Map) {
    x as immutable.Map;
  }
}

function class_explicit() {
  declare const x: unknown;

  class B {}

  var A = {B};

  if (x instanceof A.B) {
    x as empty; //error
    x as B;
  }
}

function class_util() {
  declare const x: unknown;

  class B {}

  declare const A: {B: Class<B>};

  if (x instanceof A.B) {
    x as empty; //error
    x as B;
  }
}

function class_util_chain() {
  declare const x: unknown;

  class B {}

  declare const A: ?{B: Class<B>};

  if (x instanceof A?.B) {
    //error
    x as empty; //error
    x as B;
  }
}

interface MultiParentLeft {
  left: string;
}

interface MultiParentRight {
  right: string;
}

interface MultiParentBase extends MultiParentLeft, MultiParentRight {
  base: string;
}

class MultiParentChild implements MultiParentBase {
  left: string = '';
  right: string = '';
  base: string = '';
  child: string = '';
}

declare const multiParentBase: MultiParentBase;

if (multiParentBase instanceof MultiParentChild) {
  multiParentBase.child as string;
  multiParentBase.child as number; // error
  multiParentBase as empty; // error
}

if (!(multiParentBase instanceof MultiParentChild)) {
  multiParentBase.base as string;
  multiParentBase.base as number; // error
}

interface MultiParentUnrelated {
  unrelated: boolean;
}

declare const multiParentUnion: MultiParentBase | MultiParentUnrelated;

if (multiParentUnion instanceof MultiParentChild) {
  multiParentUnion.child as string;
  multiParentUnion.unrelated; // error
}

class SecondParent {
  second: string = '';
}

interface FirstParent {
  first: string;
}

interface ChildOfSecondParent extends FirstParent, SecondParent {
  childOfSecond: boolean;
}

declare const childOfSecondParent: ChildOfSecondParent;

if (childOfSecondParent instanceof SecondParent) {
  childOfSecondParent.childOfSecond as boolean;
  childOfSecondParent as empty; // error
}

if (!(childOfSecondParent instanceof SecondParent)) {
  childOfSecondParent.noSuchProp;
}
