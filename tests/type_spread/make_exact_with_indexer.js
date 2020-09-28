//@flow

type A = {[string]: number};
type B = {| ...A, ...A |};
const a: A = {foo: 3}; // Ok
