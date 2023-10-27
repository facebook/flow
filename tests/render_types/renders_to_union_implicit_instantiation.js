// This test ensures that the RendersT ~> UnionT subtyping rule correctly constraints
// tvars when the UnionT includes instantiable tvars.
component Foo() { return null }

let arr: Array<renders Foo> = [];
arr = arr.concat(arr); // NO ERROR!
