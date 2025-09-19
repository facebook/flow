import type { exportedType } from "./readonly";

type A = $ReadOnly<[string, number]>; // ERROR
type B = $ReadOnly<[number]>; // ERROR

let a: exportedType = {b: 1}; // ERROR
