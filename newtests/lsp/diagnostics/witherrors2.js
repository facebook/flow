// @thisWillBeFlowInTest

type A = {| x: A1; |}
type A1 = {| y: A2; |}
type A2 = {| z: string; |}
type B = {| x: B1; |}
type B1 = {| y: B2; |}
type B2 = {| z: number; |}
const b = ((null: any): B);
const a = (b: A);
