//@flow

//  we shouldn't suggest to shadow an existing variable
var foo = 1;
//     ^

// it makes sense to suggest existing variables in assignments
foo = 2;
// ^

// we shouldn't suggest that the param shadow an existing variable
function bar() {}
//           ^
