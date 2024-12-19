//@flow
// even though `this` is valid anywhere in the program,
// we only want to suggest it when it doesn't refer to the global object

let x = t // should NOT suggest "this"
//       ^

function foo() {
     // should suggest "this"
// ^
}

var y = (z: empty) => t // should NOT suggest "this"
//                     ^

class Bar {
  baz: any =  // should suggest "this"
//           ^
}
