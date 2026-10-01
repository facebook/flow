//@flow

interface A {}
interface B {}

function spread<A extends interface {}, B extends interface {}>(x: A, y: B): {...A, ...B, ...} {
  return null as any;
}

declare const a: A;
declare const b: B;

spread<A, B>(a, b); // OK

type X = {...A, ...B, ...}; // OK

declare const x: X;
x as any;

type Y = {...A, foo: number, ...}; // OK
declare const y: Y;
y as any;

type Z = {foo: number, ...A, ...}; // OK
declare const z: Z;
z as any;

// Instances and classes can be spread:
class F {}
type G = {...F, ...Class<F>, ...}; // Ok
declare const g: G;
g as any;
