function test1(gen: Generator<void, string, void>) {
  // The value passed to return must have the Generator's return type, as in
  // TypeScript
  var ret = gen.return(0); // error: number ~> string
  ret.value as void; // error: string ~> void
}

// However, a generator can "refuse" the return by catching an exception and
// yielding or returning internally.
function *refuse_return() {
  try {
    yield 1;
  } finally {
    return 0;
  }
}
var refuse_return_gen = refuse_return();
var refuse_return_result = refuse_return_gen.return("string"); // error: string ~> number
if (refuse_return_result.done) {
  refuse_return_result.value as string; // error: number | void ~> string
}
