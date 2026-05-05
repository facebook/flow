//@flow

function corrupt<S extends string>(x: S): S {
  return "A" + x;
}

var x: "B" = corrupt<"B">("B")
