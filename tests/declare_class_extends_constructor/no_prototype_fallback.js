// A polymorphic construct signature is still the base without type arguments;
// the `prototype` fallback must not kick in.
class PromiseSubclassNoTargs extends Promise {}
new PromiseSubclassNoTargs(resolve => resolve(1)) as PromiseSubclassNoTargs; // ok

class MapSubclassNoTargs extends Map {}
new MapSubclassNoTargs() as MapSubclassNoTargs; // ok

// As in TypeScript, a `prototype` of type `any` gives no instance to inherit.
interface AnyPrototypeConstructor {
  readonly prototype: any;
  (): void;
}
declare var AnyPrototype: AnyPrototypeConstructor;
declare class AnyPrototypeSubclass extends AnyPrototype {} // error: not inheritable

declare const unknownValue: unknown;
if (unknownValue instanceof AnyPrototype) {
  unknownValue as empty; // ok: no instance type to narrow to
}
