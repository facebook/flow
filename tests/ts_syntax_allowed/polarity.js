class Covariant<out T> {
    readonly prop: T;
    -bad: T; // error
}
class Contravariant<in T> {
    -prop: T;
    +bad: T; // error
}
class Invariant<in out T> {
    prop: number;
}

const covariant = new Covariant<'foo'>;
covariant as Covariant<string>; // ok
covariant as Covariant<'bar'>; // error
covariant.prop; // ok
covariant.prop = 'foo'; // error

const contravariant = new Contravariant<string>;
contravariant as Contravariant<'foo'>; // ok
contravariant as Contravariant<mixed>; // error
contravariant.prop = 'foo'; // ok

const invariant = new Invariant<'foo'>;
invariant as Invariant<string>; // error

type RO<out T> = {readonly [K in keyof T]: T[K]};
declare const ro_obj: RO<{a: number, b: string}>;
ro_obj.a as number; // ok
ro_obj.a = 42; // error
ro_obj.b = 'hello'; // error

import * as exported from './exported';
const imported_covariant = new exported.Covariant<'foo'>;
imported_covariant as exported.Covariant<string>; // ok
imported_covariant as exported.Covariant<'bar'>; // error
imported_covariant.prop; // ok
imported_covariant.prop = 'foo'; // error

const imported_contravariant = new exported.Contravariant<string>;
imported_contravariant as exported.Contravariant<'foo'>; // ok
imported_contravariant as exported.Contravariant<mixed>; // error
imported_contravariant.prop = 'foo'; // ok

const imported_invariant = new exported.Invariant<'foo'>;
imported_invariant as exported.Invariant<string>; // error

declare const ro_obj2: exported.RO<{a: number, b: string}>;
ro_obj2.a as number; // ok
ro_obj2.a = 42; // error
ro_obj2.b = 'hello'; // error
