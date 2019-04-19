//@flow
function f<A:string, B:string>(a: A, b: B): A {
  return a && b
}
//var x: number = f(14, "broken");
var y: "a" = f<"a", "b">("a", "b");
