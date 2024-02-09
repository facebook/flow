declare const Ts: Array<T>; // should error but doesn't
type T = (typeof Ts)[number]; // should error but doesn't

function readT(t: T) {
  t // repositioning here shouldn't cause crashing
}
