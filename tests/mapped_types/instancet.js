interface I {
  foo: number;
}

// InstanceTs have several weird behaviors both at runtime and at the type-level
// that make them impractical for mapped types. In particular, indexed access on
// InstanceTs will only consult the indexer unless there is a syntactic string literal
// present, so the definition below would emit a missing-indexer error message.
// Since they are so poorly supported with typical mapped type syntax, we intentionally
// do not allow you to use them for mapped type sources.
type MappedInterface = {[key in keyof I]: I[key]}; // ERROR, cannot map interfaces
declare const i: MappedInterface;
(i: empty); // No error

class A {
  static bar: number;
  foo: number;
}

// The same applies to instances!
type MappedInstance = {+[key in keyof A]: A[number]};
declare const inst: MappedInstance;
(inst: empty); // OK!

// Classes work fine though!
type MappedClass = {+[key in keyof Class<A>]: Class<A>[number]};
declare const c: MappedClass;
(c: {+bar: number, ...}); // OK!
