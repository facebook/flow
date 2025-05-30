var x0: $NonMaybeType<number|string> = 0; // ok, number ~> number|string
var x1: $NonMaybeType<number|string> = true; // err, boolean ~> number|string
var x2: ({p:number}|{p:string})["p"] = 0; // ok, number ~> number|string
var x3: ({p:number}|{p:string})["p"] = true; // err, boolean ~> number|string

// annots
type P2 = T['p']; // NB: T is not resolved yet
declare var p2: P2;
(0: P2); // ok, number ~> string|number
(null: P2); // err, null ~> string|number
type T = {p: string} | {p: number}; // NB: T resolved here

// Order
 type TupleUnion = [1] | [2];
{
  declare const x: [3];
  (x: TupleUnion); // ERROR: 1, 2
  (x: $ReadOnly<TupleUnion>); // ERROR: 1, 2 (same order as above)
}
