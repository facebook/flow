// @flow

import {
  A as AImported,
  B as BImported,
  C as CImported,
  D as DImported,
  E as EImported,
  F as FImported,
  type I as IImported,
} from './this_exports';

function test1() {
  declare class A {
    isB(): this is B; // okay
    isB1(x: number): this is B; // okay
    isB1Impl(x: number): implies this is B; // okay
    isD(): this is D; // TODO should we error when D is not a subclass of A?
    isB_prop: (this: A, x: number) => this is B; // error only on methods
    static isB_static(): this is B; // error only on instance methods
  }

  declare class B extends A {}
  declare class C extends B {}
  declare class D {}

  declare class E extends A {
    isB(): this is C; // error type guard is in invariant position
    isBImplies(): implies this is C; // okay type guard is in covariant position
  }

  interface I {
    isB(): this is B; // okay
  }

  declare var a: A;
  if (a.isB()) {
    a as B;
  } else {
    a as A;
    a as B; // error
  }
  if (a.isB1(1)) {
    a as B; // okay
  }

  declare var b: B;
  if (b.isB()) {
    b as B;
  } else {
    b as empty; // okay
  }
  if (b.isB1(1)) {
    b as B;
  }
  if (b.isB1Impl(1)) {
    b as B; // okay
  } else {
    b as empty; // error - not refining else branch
  }
  if (b.isD()) {
    b as D; // okay b is B & D
  } else {
    b as B; // okay
    b as D; // error
  }
  if (B.isB_static()) { // should not refine
    B as B; // error class B ~> B
  }

  declare var c: C;
  if (c.isB()) {
    c as B;
    c as C; // okay
  } else {
    c as empty;
  }
  if (c.isB1(1)) {
    c as B;
  }

  declare var i: I;
  if (i.isB()) {
    i as B; // okay
  } else {
    i as B; // error
  }

  function unbind(x: A) {
    const isB = x.isB // error method-unbinding
    isB as () => boolean; // okay (isB still shows as a type guard)
  }
}

function test2() {
  class C {}

  type T1 = (x: mixed) => this is C; // error only on declare classes/interfaces

  type T2 = (cb: (x: mixed) => this is C) => void; // error only on declare classes/interfaces

  type T3 = (x: mixed) => this is Unresolved; // error only on declare classes/interfaces, Unresolved

  type T4 = {
    m(): this is C; // error only on declare classes/interfaces
    f: () => this is C; // error only on declare classes/interfaces
  };

  declare var x: T4;
  if (x.m()) {
    x as C; // error should not refine
  }

  declare class D {
    m(cb: (x: mixed) => this is C): void; // error only on declare classes/interfaces method return
    n(): (x: mixed) => this is C; // error only on declare classes/interfaces method return
  }

  function foo1(this: C): this is C {  // error 'this' type guard only on declare classes
    return 0 as any;
  }

  function foo2(this: C): this is Unresolved {  // error 'this' type guard only on declare classes, unresolved
    return 0 as any;
  }
}

function test3() {
  declare class C<X> {
    m1(): this is D<X>;
    m2<Y>(): this is D<Y>;
    m3<Y>(y: Y): this is D<Y>;
    m4<Y>(): this is Y;
    m5<Y>(y: Y): this is Y;
  }

  declare class D<+X> extends C<X> {}

  declare var x: C<number>;
  if (x.m1()) {
    x as C<number>; // okay
    x as D<number>; // okay
    x as D<string>; // error number ~> string
  }

  if (x.m2()) {
    x as D<mixed>;
  }

  if (x.m2<number>()) {
    x as D<number>;
    x as D<string>; // error number ~> string
  }

  if (x.m3(1)) {
    x as D<number>;
    x as D<string>; // error number ~> string
  }

  if (x.m4()) {
    x as C<number>; // okay
  }

  if (x.m4<D<number>>()) {
    x as D<number>; // okay
  }

  if (x.m5(new D())) {
    x as D<mixed>; // okay
  }
}

function test4() {
  declare class B { m(): boolean }
  declare class C extends B { m(): this is D }
  declare class D extends C {}
  declare function fn(cb: interface { m(): this is D }): void;

  fn(new C); // okay
  fn(new D); // okay
  fn(new B); // error no type-guard
}

function test5() {
  declare class E extends AImported {
    isB(): this is CImported; // error type guard is in invariant position
    isBImplies(): implies this is CImported; // okay type guard is in covariant position
  }

  declare var a: AImported;
  if (a.isB()) {
    a as BImported;
  } else {
    a as AImported;
    a as BImported; // error
  }
  if (a.isB1(1)) {
    a as BImported; // okay
  }

  declare var b: BImported;
  if (b.isB()) {
    b as BImported;
  } else {
    b as empty; // okay
  }
  if (b.isB1(1)) {
    b as BImported;
  }
  if (b.isB1Impl(1)) {
    b as BImported; // okay
  } else {
    b as empty; // error - not refining else branch
  }
  if (b.isD()) {
    b as DImported; // okay b is BImported & DImported
  } else {
    b as BImported; // okay
    b as DImported; // error
  }
  if (BImported.isB_static()) { // should not refine
    BImported as BImported; // error class B ~> B
  }

  declare var c: CImported;
  if (c.isB()) {
    c as BImported;
    c as CImported; // okay
  } else {
    c as empty;
  }
  if (c.isB1(1)) {
    c as BImported;
  }

  function unbind(x: AImported) {
    const isB = x.isB // error method-unbinding
    isB as () => boolean; // okay (isB still shows as a type guard)
  }

  declare var e: EImported;
  if (e.isF()) {
    e as FImported; // okay this type guard is still recorded in regular class
  } else {
    e as EImported;
    e as FImported; // error
  }

  declare var i: IImported;
  if (i.isB()) {
    i as BImported; // okay
  } else {
    i as BImported; // error
  }
}

function test6() {
  // This is a limitation that does not allow the same variable to be a candidate
  // for regular parameter and this-parameter refinement. E.g. in `x.f(x)` we only
  // consider `x` for regular parameter refinement, not this-refinement.
  //
  // This is not fundamentally impossible, just causes some issues with the
  // name_resolver.

  declare class A {
    f(x: mixed): this is B;
  }
  declare class B extends A {}

  declare var x: A;

  if (x.f(x)) {
    x as A
    x as B; // TODO okay (relax this limitation)
  }

  declare class C {
    f(x: mixed): x is B;
  }

  declare var y: C;

  if (y.f(y)) {
    y as A
    y as B; // okay -- y as parameter is still refined
  }
}
