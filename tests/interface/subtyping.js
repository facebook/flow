//@flow

interface I {
  [string]: number;
  constructor(): void;
}

class C {
  x: number;
}

new C() as I;

interface UnknownIndexer {
  [string]: unknown;
}

declare class Window {
  foo: string;
}

declare const window: Window;

window as UnknownIndexer; // error, `foo` is invariant

class BaseWithStringField {
  foo: string;
}

class DerivedWithStringField extends BaseWithStringField {}

new DerivedWithStringField() as interface {[string]: number}; // error, inherited `foo` is incompatible

declare class C2 {
  [string]: number;
  x: number;
}

new C2() as I;

class A {
  x: number;
}

class B extends A {
  y: number;
}

interface J {
  x: number;
  y: number;
}

new A() as J; // error
new B() as J;

class D {
  static x: number;
}

interface K {
  x: number;
}

new D() as K; // error
D as K;

class E {
  static x: number;
}

class F extends E {
  static y: number;
}

interface L {
  x: number;
  y: number;
}

E as L; // error
F as L;

class G {}

interface M {
  z: number;
}

interface N extends M {}

new G() as M; // error
new G() as N; // error

declare var SelfIndexer: Class<SelfIndexed>;

class SelfIndexed extends SelfIndexer {
  foo: string;
}

declare const selfIndexed: SelfIndexed;

selfIndexed as interface {[string]: number};

declare const uint8Array: Uint8Array;
uint8Array as $ArrayLike<number>;

declare class NamedPropertyOnly {
  foo: string;
}
new NamedPropertyOnly() as interface {[number]: number};

declare class NumericProperty {
  0: string;
}
new NumericProperty() as interface {[number]: number}; // error
