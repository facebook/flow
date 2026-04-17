declare function f<in A>(x: A): void; // ERROR - in / out not allowed in function parameters
declare function f<out A>(): A; // ERROR - in / out not allowed in function parameters
declare function f<in out A>(x: A): A; // ERROR

declare function f<in>(x: in): void; // OK
declare function f<out>(x: out): void; // OK
const out = 1; // OK

type Contravariant<in A> = (A) => void; // OK
type Covariant<out A> = {readonly prop: A}; // OK
interface ContravariantInterface<in A> { method(A): void } // OK
interface CovariantInterface<out A> { readonly prop: A } // OK
type ContravariantWriteonly<in A> = {-prop: A}; // OK // TODO: replace - with writeonly once implemented in the parser

type BadOut<out A> = (A) => void; // ERROR: out in contravariant position
type BadIn<in A> = {readonly prop: A}; // ERROR: in in covariant position
interface BadOutInterface<out A> { method(A): void } // ERROR: out in contravariant position
interface BadInInterface<in A> { readonly prop: A } // ERROR: in in covariant position
type BadInWriteonly<out A> = {-prop: A}; // ERROR: out in contravariant position // TODO: replace - with writeonly once implemented

type InOut<in out A> = A; // ERROR: in out is always unsupported
interface InOutInterface<in out A> { prop: A } // ERROR: in out is always unsupported
