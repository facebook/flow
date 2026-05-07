//@flow

interface A {}
interface B {}

function spread<A extends interface {}, B extends interface {}>(x: A, y: B): {...A, ...B, ...} {
  return null as any;
}

declare const a: A;
declare const b: B;

spread<A, B>(a, b); // Error, can't spread interface

type X = {...A, ...B, ...}; // Error, can't spread interface

declare const x: X;
x as any;

type Y = {...A, foo: number, ...}; // Error, can't spread interface
declare const y: Y;
y as any;

type Z = {foo: number, ...A, ...}; // Error, can't spread interface
declare const z: Z;
z as any;

// Instances and classes can be spread:
class F {}
type G = {...F, ...Class<F>, ...}; // Ok
declare const g: G;
g as any;
