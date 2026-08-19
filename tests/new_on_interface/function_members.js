// A construct signature makes the interface a function at runtime, so it has
// `Function`'s members — the same augment TypeScript applies by falling back to
// `NewableFunction` when a type has construct signatures.

interface Bar {
  b: string;
}
interface BarCtor {
  new (x: string): Bar;
}
declare const BarC: BarCtor;

BarC.name as string; // OK
BarC.length as number; // OK
BarC.bind; // OK
BarC.nope; // ERROR: not a `Function` member either

// Inherited through the extends chain: only the ancestor that declares the
// signature needs to reach `Function.prototype`.
interface InheritedCtor extends BarCtor {}
declare const InheritedC: InheritedCtor;
InheritedC.name as string; // OK

// A call property already did this; a construct signature now behaves the same.
interface Callable {
  (): void;
}
declare const callable: Callable;
callable.name as string; // OK

// Without either, an interface is a plain object.
interface Plain {
  b: string;
}
declare const plain: Plain;
plain.name; // ERROR
