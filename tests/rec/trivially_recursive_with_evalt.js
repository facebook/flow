declare const Ts: Array<T>; // errored below
type T = (typeof Ts)[number]; // error: cyclic

function readT(t: T) {
  t // repositioning here shouldn't cause crashing
}

type Props = { ...Props }; // error: cyclic

function readProps(props: Props) {
  props // repositioning here shouldn't cause crashing
}

type C = 0 extends number ? C : C; // error: cyclic

function readC(c: C) {
  c // repositioning here shouldn't cause crashing
}
