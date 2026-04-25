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
type ContravariantWriteonly<in A> = { writeonly prop: A }; // OK

type BadOut<out A> = (A) => void; // ERROR: out in contravariant position
type BadIn<in A> = {readonly prop: A}; // ERROR: in in covariant position
interface BadOutInterface<out A> { method(A): void } // ERROR: out in contravariant position
interface BadInInterface<in A> { readonly prop: A } // ERROR: in in covariant position
type BadInWriteonly<out A> = { writeonly prop: A }; // ERROR: out in contravariant position

type InOut<in out A> = A; // ERROR: in out is always unsupported
interface InOutInterface<in out A> { prop: A } // ERROR: in out is always unsupported

// Function type annotations (distinct from type aliases evaluating to function types)
type FnIn = <in T>(x: T) => void; // ERROR - in / out not allowed on function type parameters
type FnOut = <out T>() => T; // ERROR - in / out not allowed on function type parameters

// Method type parameters in classes
class MethodHost {
  contraMethod<in T>(x: T): void {} // ERROR - in / out not allowed on method type parameters
  coMethod<out T>(): T { throw 0; } // ERROR - in / out not allowed on method type parameters
}

// Method type parameters in interfaces
// TODO: should ERROR like the class-method case above, but currently accepted
// silently because mk_method_func_sig in type_annotation.ml does not call
// error_on_unsupported_variance_annotation. Will be fixed in a follow-up diff.
interface MethodInterface {
  contraMethod<in T>(x: T): void; // OK (TODO: should ERROR)
  coMethod<out T>(): T; // OK (TODO: should ERROR)
}

// Same restriction applies to the +/- variance spellings (no flag required)
type FnPlus = <+T>(x: T) => void; // ERROR - variance modifiers not allowed on function type parameters
type FnMinus = <-T>(x: T) => void; // ERROR - variance modifiers not allowed on function type parameters

class MethodHostShortForm {
  contraMethod<-T>(x: T): void {} // ERROR - variance modifiers not allowed on method type parameters
  coMethod<+T>(): T { throw 0; } // ERROR - variance modifiers not allowed on method type parameters
}

// Same TODO: interface methods silently accept +/- variance markers too.
interface MethodInterfaceShortForm {
  contraMethod<-T>(x: T): void; // OK (TODO: should ERROR)
  coMethod<+T>(): T; // OK (TODO: should ERROR)
}
