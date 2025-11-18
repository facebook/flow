import type { exportedType } from "./readonly";

type A = $ReadOnly<[string, number]>; // OK
type B = $ReadOnly<[number]>; // OK

let a: exportedType = {b: 1}; // ERROR

import type { exportedValuesType } from "./values";

let b: exportedValuesType = 3; // ERROR
