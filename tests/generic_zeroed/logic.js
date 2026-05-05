//@flow
function f<A extends string, B extends string>(a: A, b: B): A {
  return a && b
}
//var x: number = f(14, "broken");
var y: "a" = f<"a", "b">("a", "b");

function compareGeneric<T extends number>(a: T, b: T): boolean {
  return a < b;
}

function compareGeneric2<T extends number, S extends number>(a: T, b: S): boolean {
  return a < b;
}
