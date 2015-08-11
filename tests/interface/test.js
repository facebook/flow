/* @flow */

type AI = class {
  aMethod1(): number;
  static asMethod1(): string;
};

class A {
  constructor() {};
  aMethod1(): number {
    return 13;
  }
  aMethod2(): string {
    return "from A's nonstatic";
  }
  static asMethod1(): string {
    return "from A's static";
  }
  static asMethod2(): number {
    return 7;
  }
}

type BI = class {
  bMethod1(): number;
  static bsMethod1(): number;
};

class B {
  constructor() {};
  bMethod1(): number {
    return 7;
  };
  bMethod2(): string {
    return "a string";
  };
  static bsMethod1(): number {
    return 123;
  };
  static bsMethod2(): string {
    return "another string";
  }
}

class C extends B {
  a(): number {
    return 22;
  }
  static c(): string {
    return "c";
  }
}

class D extends C {
  aMethod1(): number {
    return 98;
  }
  static asMethod1(): string {
    return "asMethod on D";
  }
}

type EI<T> = class EI {
  eMethod1(): T;
  static esMethod1(): Class<T>;
}

class E { // implements EI<B>
  eMethod1() {
    return new B();
  }
  eMethod2() {
    return 5;
  }
  static esMethod1() {
    return B;
  }
  static esMethod2() {
    return "e static";
  }
}

class F {
  eMethod1() { // implements EI<D>, EI<C>, EI<B>
    return new D();
  }
  eMethod2() {
    return "f method";
  }
  static esMethod1() {
    return D;
  }
  static esMethod2() {
    return 1;
  }
}

function b1(ell: BI): number {
  return ell.bMethod1();
}

function b2(Ell: Class<B>): number {
  return Ell.bsMethod1();
}

function b3(Ell: Class<BI>): number {
  return Ell.bsMethod1();
}

function ab1(m: AI&BI): number {
  return m.aMethod1() + m.bMethod1();
}

function ab2(M: Class<AI>&Class<BI>): string {
  return M.asMethod1() + M.bsMethod1();
}

function e1(N: Class<EI<B>>): string {
  var Bp = N.esMethod1();
  return Bp.bsMethod1() + Bp.bsMethod2();
}

function e2(n: EI<B>): string {
  var b = n.eMethod1();
  return b.bMethod1() + b.bMethod2();
}

var b: BI = new B();
var x1 = b1(b);
var x2 = b2(C);
var x3 = b3(C);
var d = new D();
var y1 = ab1(d);
var y2 = ab2(D);
var e: EI<B> = new F();
//var f: EI<D> = new E(); // OK error: E returns B's, which may not have all of the methods of D's
var z1 = e.eMethod1();
var z2 = e.eMethod1(b,d); // Too many arguments => okay by Flow.
var Z3 = e.constructor.esMethod1();
var Z4 = e.constructor.esNonmethod(b,d); // NG: Type checks despite bad name.
var Z5 = E.esMethod1();
var Z6 = F.esMethod1();

var z7 = e1(E);
/*
 * If I put this assignment prior to `var f...`, then the `var f` error makes no
 * mention of `z7`'s site:
test.js:29:7,7: B
This type is incompatible with
test.js:54:7,7: D

test.js:118:25,25: B
This type is incompatible with
test.js:54:7,7: D
Error path:
:                   MixedT [Object]
test.js:54:7,7:     ~> ExtendsT [extends D] comes from
test.js:118:25,25:  . InstanceT [B]
test.js:54:7,7:     . ~> ExtendsT [extends D] comes from
test.js:118:25,25:  . . InstanceT [B]
test.js:54:7,7:     . . ~> InstanceT [D] comes from
test.js:118:25,25:  . . . OpenT [B]
test.js:54:7,7:     . . . ~> InstanceT [D] comes from
test.js:118:25,25:  . . . . AnnotT [B]
test.js:54:7,7:     . . . . ~> InstanceT [D] comes from
test.js:118:25,25:  . . . . . AnnotT [B]
test.js:143:11,11:  . . . . . ~> OpenT [D] comes from
test.js:118:25,25:  . . . . . . AnnotT [B]
test.js:143:11,11:  . . . . . . ~> AnnotT [D] comes from
test.js:64:3,15:    . . . . . . . FunT [function type]
test.js:69:3,71:3:  . . . . . . . ~> FunT [method eMethod1] comes from
test.js:142:13,13:  . . . . . . . . InstanceT [E]
test.js:64:3,15:    . . . . . . . . ~> LookupT [property eMethod1] comes from
test.js:142:13,13:  . . . . . . . . . InstanceT [E]
test.js:63:14,66:1: . . . . . . . . . ~> ClsT [EI] comes from
test.js:142:13,13:  . . . . . . . . . . InstanceT [E]
test.js:142:13,13:  . . . . . . . . . . ~> OpenT [E] comes from
test.js:142:13,13:  . . . . . . . . . . . InstanceT [E]
test.js:142:13,13:  . . . . . . . . . . . ~> AnnotT [E] comes from
test.js:142:13,13:  . . . . . . . . . . . . ShiftT [class type: E]
test.js:118:22,26:  . . . . . . . . . . . . ~> ShiftT [shifted type: type application of polymorphic type: type EI] comes from
test.js:142:10,11:  . . . . . . . . . . . . . FunT [function]
test.js:142:10,14:  . . . . . . . . . . . . . ~> CallT [function call]
test.js:69:3,71:3:  . . . . . . . FunT [method eMethod1]
test.js:64:3,15:    . . . . . . . ~> FunT [function type] comes from
test.js:68:7,7:     . . . . . . . . InstanceT [E]
test.js:64:3,15:    . . . . . . . . ~> LookupT [property eMethod1] comes from
test.js:68:7,7:     . . . . . . . . . InstanceT [E]
test.js:63:14,66:1: . . . . . . . . . ~> ClsT [EI] comes from
test.js:68:7,7:     . . . . . . . . . . InstanceT [E]
test.js:68:7,7:     . . . . . . . . . . ~> OpenT [E] comes from
test.js:68:7,7:     . . . . . . . . . . . InstanceT [E]
test.js:68:7,7:     . . . . . . . . . . . ~> AnnotT [E] comes from
test.js:68:7,7:     . . . . . . . . . . . . InstanceT [E]
test.js:143:8,12:   . . . . . . . . . . . . ~> TypeAppT [type application of type EI] comes from
test.js:143:16,22:  . . . . . . . . . . . . . VoidT [undefined]
test.js:68:7,7:     . . . . . . . . . . . . . ~> ObjTestT [constructor return] comes from
test.js:68:7,7:     . . . . . . . . . . . . . . FunT [default constructor]
test.js:143:16,22:  . . . . . . . . . . . . . . ~> CallT [constructor call] comes from
test.js:68:7,7:     . . . . . . . . . . . . . . . InstanceT [E]
test.js:143:16,22:  . . . . . . . . . . . . . . . ~> MethodT [constructor call] comes from
test.js:68:7,7:     . . . . . . . . . . . . . . . . ShiftT [class type: E]
test.js:143:16,22:  . . . . . . . . . . . . . . . . ~> ConstructorT [constructor call]
test.js:68:7,7:     . . . . . . . . . . . . . . ShiftT [class type: E]
test.js:143:16,22:  . . . . . . . . . . . . . . ~> ConstructorT [constructor call]
 *
 * However, if I leave this assignment here, then the `var f` error attaches the
 * second message to the `z7` function call site:
 *
test.js:29:7,7: B
This type is incompatible with
test.js:54:7,7: D

test.js:150:10,14: function call
Error:
test.js:118:25,25: B
This type is incompatible with
test.js:54:7,7: D
Error path:
:                   MixedT [Object]
test.js:54:7,7:     ~> ExtendsT [extends D] comes from
test.js:118:25,25:  . InstanceT [B]
test.js:54:7,7:     . ~> ExtendsT [extends D] comes from
test.js:118:25,25:  . . InstanceT [B]
test.js:54:7,7:     . . ~> InstanceT [D] comes from
test.js:118:25,25:  . . . OpenT [B]
test.js:54:7,7:     . . . ~> InstanceT [D] comes from
test.js:118:25,25:  . . . . AnnotT [B]
test.js:54:7,7:     . . . . ~> InstanceT [D] comes from
test.js:118:25,25:  . . . . . AnnotT [B]
test.js:142:11,11:  . . . . . ~> OpenT [D] comes from
test.js:118:25,25:  . . . . . . AnnotT [B]
test.js:142:11,11:  . . . . . . ~> AnnotT [D] comes from
test.js:64:3,15:    . . . . . . . FunT [function type]
test.js:69:3,71:3:  . . . . . . . ~> FunT [method eMethod1] comes from
test.js:150:13,13:  . . . . . . . . InstanceT [E]
test.js:64:3,15:    . . . . . . . . ~> LookupT [property eMethod1] comes from
test.js:150:13,13:  . . . . . . . . . InstanceT [E]
test.js:63:14,66:1: . . . . . . . . . ~> ClsT [EI] comes from
test.js:150:13,13:  . . . . . . . . . . InstanceT [E]
test.js:150:13,13:  . . . . . . . . . . ~> OpenT [E] comes from
test.js:150:13,13:  . . . . . . . . . . . InstanceT [E]
test.js:150:13,13:  . . . . . . . . . . . ~> AnnotT [E] comes from
test.js:150:13,13:  . . . . . . . . . . . . ShiftT [class type: E]
test.js:118:22,26:  . . . . . . . . . . . . ~> ShiftT [shifted type: type application of polymorphic type: type EI] comes from
test.js:150:10,11:  . . . . . . . . . . . . . FunT [function]
test.js:150:10,14:  . . . . . . . . . . . . . ~> CallT [function call]
test.js:69:3,71:3:  . . . . . . . FunT [method eMethod1]
test.js:64:3,15:    . . . . . . . ~> FunT [function type] comes from
test.js:142:20,20:  . . . . . . . . InstanceT [E]
test.js:64:3,15:    . . . . . . . . ~> LookupT [property eMethod1] comes from
test.js:142:20,20:  . . . . . . . . . InstanceT [E]
test.js:63:14,66:1: . . . . . . . . . ~> ClsT [EI] comes from
test.js:142:20,20:  . . . . . . . . . . InstanceT [E]
test.js:142:20,20:  . . . . . . . . . . ~> OpenT [E] comes from
test.js:142:20,20:  . . . . . . . . . . . InstanceT [E]
test.js:142:20,20:  . . . . . . . . . . . ~> AnnotT [E] comes from
test.js:142:20,20:  . . . . . . . . . . . . InstanceT [E]
test.js:142:8,12:   . . . . . . . . . . . . ~> TypeAppT [type application of type EI] comes from
test.js:142:16,22:  . . . . . . . . . . . . . VoidT [undefined]
test.js:142:20,20:  . . . . . . . . . . . . . ~> ObjTestT [constructor return] comes from
test.js:68:7,7:     . . . . . . . . . . . . . . FunT [default constructor]
test.js:142:16,22:  . . . . . . . . . . . . . . ~> CallT [constructor call] comes from
test.js:142:20,20:  . . . . . . . . . . . . . . . InstanceT [E]
test.js:142:16,22:  . . . . . . . . . . . . . . . ~> MethodT [constructor call] comes from
test.js:142:20,20:  . . . . . . . . . . . . . . . . ShiftT [class type: E]
test.js:142:16,22:  . . . . . . . . . . . . . . . . ~> ConstructorT [constructor call]
test.js:142:20,20:  . . . . . . . . . . . . . . ShiftT [class type: E]
test.js:142:16,22:  . . . . . . . . . . . . . . ~> ConstructorT [constructor call]
 *
 * These errors are verbatim identical except for the first two lines.  This
 * looks like a bug in the error handling.  If the EI<E> was cached at the
 * signature of `e1` and then reused, then this might arise?  Save it for later
 * when prettying up the error messages.
 */

var z8 = e1(F);
var z9 = e2(e);
/*
 * Operator `Interface<.>` would be handy for stripping an interface from a
 * class and for shifting interfaces (Class<SomeInterface> errors).
 */
