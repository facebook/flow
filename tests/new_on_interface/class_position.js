// An interface with a construct signature can be used where a class is
// expected. Nothing here is TypeScript: `lib/core.js` declares the typed array
// constructors this way, so plain Flow interfaces must work too.

interface Bar {
  b: string;
}

interface BarCtor {
  new (x: string): Bar;
}
declare const BarC: BarCtor;

BarC as Class<Bar>; // OK
BarC as Class<any>; // OK: constructability alone is enough for an untyped class consumer

interface Wrong {
  wrong: number;
}
interface WrongCtor {
  new (): Wrong;
}
declare const WrongC: WrongCtor;
WrongC as Class<Bar>; // ERROR: the construct signature returns Wrong, not Bar

// A construct signature inherited from a supertype counts: the same lookup
// backs `new InheritedC()`.
interface InheritedCtor extends BarCtor {}
declare const InheritedC: InheritedCtor;
InheritedC as Class<Bar>; // OK
new InheritedC('x') as Bar; // OK

// Own and inherited signatures are pooled into one overload set.
interface RedeclaredCtor extends BarCtor {
  new (x: string, y: number): Bar;
}
declare const RedeclaredC: RedeclaredCtor;
RedeclaredC as Class<Bar>; // OK

interface InheritsWrongCtor extends WrongCtor {}
declare const InheritsWrongC: InheritsWrongCtor;
InheritsWrongC as Class<Bar>; // ERROR: the inherited signature returns Wrong, not Bar

// Overload resolution: one matching branch is enough.
interface Str {
  s: string;
}
interface Num {
  n: number;
}
interface MultiCtor {
  new (x: string): Str;
  new (x: number): Num;
}
declare const MultiC: MultiCtor;
MultiC as Class<Str>; // OK
MultiC as Class<Bar>; // ERROR: no construct overload returns Bar

// No construct signature: still not a class.
interface Plain {
  b: string;
}
declare const plain: Plain;
plain as Class<Bar>; // ERROR

// An instance of a class is not the class itself.
declare class Cls {}
declare const cls: Cls;
cls as Class<Cls>; // ERROR
