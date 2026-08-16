// The same `unique symbol` used twice is a duplicate member, but two distinct
// symbols are not (see `local.js`, where `[key1]` and `[key2]` coexist).

declare const key1: unique symbol;

class C {
  [key1]: number = 1;
  [key1]: string = 's'; // ERROR: duplicate class member
}
