enum E {
  A,
  B,
}

const o = {
  [E.A]: 0, // OK
};

o as {[E]: number}; // OK
o[E.A] as number; // OK
o[E.B] = 1; // OK

o as {A: number, ...}; // ERROR
o[E.A] as empty; // ERROR
o['A']; // ERROR

const spread = {...o, [E.B]: 1}; // OK
spread as {[E]: number}; // OK

const err_spread_1 = {A: 1, ...o}; // ERROR
declare const s: string;
const err_spread_2 = {...o, [s]: 1}; // ERROR

// Object.key still results in `string`
const k = Object.keys(o);
k as $ReadOnlyArray<string>; // OK
k as $ReadOnlyArray<E>; // ERROR
