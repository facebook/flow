// `abstract new () => T` must preserve its abstractness through the
// type-sig pipeline so the bit survives crossing a module boundary —
// otherwise an imported abstract ctor would degrade to a non-abstract
// slot and `new x()` would be silently accepted.

import type {AbstractCtor, ConcreteCtor} from './abstract_ctor_type_cross_module_lib';

declare const ac: AbstractCtor;
declare const cc: ConcreteCtor;

ac as AbstractCtor; // OK

ac as ConcreteCtor; // ERROR: abstract ctor into non-abstract slot

new ac(); // ERROR: cannot `new` an abstract-typed constructor

new cc(); // OK
