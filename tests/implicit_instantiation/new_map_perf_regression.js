// @flow

// This test times out quite reliably without the OpenT indirection in
// Implicit_instantiation.instantiate_poly_with_subst_map

interface $Iterator_<+Yield,+Return,-Next> {
  @@iterator(): $Iterator_<Yield,Return,Next>;
}

type Iterator_<+T> = $Iterator_<T,void,void>;

interface $Iterable_<+Yield,+Return,-Next> {
  @@iterator(): $Iterator<Yield,Return,Next>;
}

type Iterable_<+T> = $Iterable_<T,void,void>;

declare class $ReadOnlyMap<K, +V> {
  @@iterator(): Iterator_<[K, V]>;
}

declare class Map<K, V> extends $ReadOnlyMap<K, V> {
  @@iterator(): Iterator_<[K, V]>;
  constructor(iterable: Iterable_<[K, V]>): void;
}

declare var n: number;

const m0 = new Map([[n, '123']]);
const m1 = new Map([[n, m0]]);
const m2 = new Map([[n, m1]]);
const m3 = new Map([[n, m2]]);
const m4 = new Map([[n, m3]]);
const m5 = new Map([[n, m4]]);
const m6 = new Map([[n, m5]]);
const m7 = new Map([[n, m6]]);
const m8 = new Map([[n, m7]]);
const m9 = new Map([[n, m8]]);
const m10 = new Map([[n, m9]]);
