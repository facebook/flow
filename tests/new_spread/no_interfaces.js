//@flow

interface A {}
interface B {}

function spread<A: interface {}, B: interface {}>(x: A, y: B): {...A, ...B} {
  return null as any;
}

declare var a: A;
declare var b: B;

spread<A, B>(a, b); // Error, can't spread interface

type X = {...A, ...B}; // Error, can't spread interface

declare var x: X;
x as any;

type Y = {...A, foo: number}; // Error, can't spread interface
declare var y: Y;
y as any;

type Z = {foo: number, ...A}; // Error, can't spread interface
declare var z: Z;
z as any;

// Instances and classes can be spread:
class F {}
type G = {...F, ...Class<F>}; // Ok
declare var g: G;
g as any;
