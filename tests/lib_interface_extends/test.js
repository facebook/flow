declare const locale: LibLocale;
locale.name as string; // OK
locale as LibOptions; // OK

declare const narrowed: LibNarrowed;
narrowed.name as string; // OK

new LibDerived().x as string; // OK

interface Options {
  name?: string;
}
interface Locale extends Options { // ERROR: writable `name` is invariant outside library definitions
  name: string;
}
interface Narrowed extends Options { // ERROR: `name` is read-only here but writable in `Options`
  readonly name: string;
}

declare class Base {
  x: string | number;
}
declare class Derived extends Base { // ERROR: writable `x` is invariant outside library definitions
  x: string;
}
