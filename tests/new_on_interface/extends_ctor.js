// A class can extend a value with a construct signature. The instance it
// inherits from is what the signature returns, which is what TypeScript's
// `resolveBaseTypesOfClass` uses. `lib/core.js` needs this: `node.js.flow`
// declares `class Buffer extends Uint8Array`, and `Uint8Array` the value is now
// `Uint8ArrayConstructor`.

interface Bar {
  b: string;
}
interface BarCtor {
  new (x: string): Bar;
}
declare const BarC: BarCtor;

declare class Derived extends BarC {
  d: number;
}
declare const derived: Derived;
derived.b as string; // OK: inherited from Bar
derived.d as number; // OK
derived.nope; // ERROR
derived as Bar; // OK: Derived is a subtype of the base instance

// `prototype` is deliberately ignored here — unlike `instanceof`, which prefers
// it. TypeScript draws the line in the same place.
interface Proto {
  p: string;
}
interface ProtoCtor {
  readonly prototype: Proto;
  new (): Bar;
}
declare const ProtoC: ProtoCtor;
declare class FromProto extends ProtoC {}
declare const fromProto: FromProto;
fromProto.b as string; // OK: the construct signature's return type
fromProto.p; // ERROR: `prototype` does not decide the base instance

// Overloads take the first signature.
interface Str {
  s: string;
}
interface MultiCtor {
  new (x: string): Str;
  new (x: number): Bar;
}
declare const MultiC: MultiCtor;
declare class FromMulti extends MultiC {}
declare const fromMulti: FromMulti;
fromMulti.s as string; // OK
fromMulti.b; // ERROR: the second overload does not contribute

// The derived class's statics inherit from the base constructor value itself,
// the way TypeScript makes `typeof Derived` extend the base constructor type.
// `node.js.flow` relies on it: `Buffer.from` has to reach `Uint8ArrayConstructor`.
interface StaticsCtor {
  new (): Bar;
  of(x: string): Bar;
  readonly tag: string;
}
declare const StaticsC: StaticsCtor;
declare class FromStatics extends StaticsC {}
FromStatics.of('x') as Bar; // OK: inherited from `StaticsCtor`
FromStatics.tag as string; // OK
FromStatics.nope; // ERROR

// No construct signature: still not inheritable.
interface Plain {
  b: string;
}
declare const plain: Plain;
declare class NotDerived extends plain {} // ERROR
