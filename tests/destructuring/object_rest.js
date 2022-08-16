type O1 = { p: number, q: number }
declare var o1: O1;
var {p:p1, ...o1_rest} = o1;
(o1_rest: { q: number }); // ok
o1_rest.x = 0; // error: rest result is sealed

type O2 = {| p: number, q: number |}
declare var o2: O2;
var {p:p2, ...o2_rest} = o2;
(o2_rest: {| q: number|}); // ok
o2_rest.x = 0; // error: rest result is sealed

var o3 = { p: 0, q: 0 };
var {p:p3, ...o3_rest} = o3;
(o3_rest: {| q: number |}); // ok
o3_rest.x = 0; // error: rest result is sealed
