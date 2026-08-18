// A Flow file consuming construct signatures declared in a `.d.ts`. The desugar
// is gated on the declaring file's extension, so everything here works even
// though this file is Flow.
import type {ArrowCtor, ICtor} from './object_construct_sig';
import {
  Bar,
  BarCtor,
  Dual,
  Field,
  Inherited,
  MismatchedPrototypeCtor,
  Multi,
  Num,
  Obj,
  OptionalMethod,
  Plain,
  Quoted,
  Str,
  WrongCtor,
} from './object_construct_sig';

new BarCtor('x') as Bar; // OK
BarCtor.of() as Bar; // OK — statics still reachable
BarCtor.prototype as Bar; // OK — fields still reachable
new BarCtor(1); // ERROR — number ~> string
new BarCtor(); // ERROR — too few arguments
new BarCtor('x').b as number; // ERROR — string ~> number

new Plain() as Bar; // OK
Plain as Class<any>; // OK: constructability alone is sufficient for an untyped class consumer.
Plain as Class<Bar>; // OK: the construct signature determines the instance type.
BarCtor as Class<Bar>; // OK: an explicit compatible prototype is not required.
MismatchedPrototypeCtor as Class<Bar>; // OK: TypeScript checks the construct signature, not the class prototype property.
WrongCtor as Class<Bar>; // ERROR: the construct signature returns Wrong, not Bar.
Inherited as Class<Bar>; // OK: inherited construct signatures participate too.

// Overloads
new Multi('s') as Str; // OK
new Multi(1) as Num; // OK
new Multi(true); // ERROR — neither overload accepts boolean
Multi as Class<Str>; // OK: TypeScript accepts a compatible source construct overload.
Multi as Class<Bar>; // ERROR: no construct overload returns Bar.

// Call property and construct signature coexist
Dual() as string; // OK
new Dual() as Bar; // OK

// Interchangeable with the interface and arrow spellings, both directions
declare const ic: ICtor;
declare const ac: ArrowCtor;
Obj as ICtor; // OK
Obj as ArrowCtor; // OK
ic as typeof Obj; // OK
ac as typeof Obj; // OK

// A generic factory parameter is satisfied by a TS constructor object, as long
// as the parameter is spelled in syntax that means "construct signature" in a
// Flow file — `new () => T` or an interface. A `{new(): T}` object type written
// here is just an object with a method named `new`, per the gate.
declare function create<T>(C: new () => T): T;
create(Plain) as Bar; // OK
declare function createViaInterface<T>(C: interface {new (): T}): T;
createViaInterface(Plain) as Bar; // OK
declare function createViaObject<T>(C: {new (): T}): T;
createViaObject(Plain); // ERROR — `{new(): T}` in a Flow file is not a construct signature

// Not construct signatures
Quoted.new() as Bar; // OK — ordinary method
new Quoted(); // ERROR: invalid-constructor
new OptionalMethod(); // ERROR: invalid-constructor
Field.new() as Bar; // OK — ordinary property
new Field(); // ERROR: invalid-constructor
