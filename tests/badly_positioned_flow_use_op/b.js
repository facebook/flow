//@flow

type Data = {| x: number |}

declare function foo(data: Data): void;

const o = {
  fun: foo,
}

function test1(b: boolean) {
  var data = { x: 0 };
  if (b) data = { x: 0, z: 0 };
  o['fun'](data); // no error
}
/*The error position for this one is ok.*/
function test2(b: boolean) {
  var data = { z: 0 };
  o['fun'](data); // Error: { z: 0 } ~> Data
}
