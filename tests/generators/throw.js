function *catch_return() {
  try {
    yield 0;
  } catch (e) {
    return e;
  }
}
var catch_return_value = catch_return().throw("").value;
invariant(typeof catch_return_value !== "undefined");
invariant(typeof catch_return_value !== "number");
(catch_return_value : boolean); // error: catch (mixed) ~> boolean

function *yield_return() {
  try {
    yield 0;
    return;
  } catch (e) {
    yield e;
  }
}
var yield_return_value = yield_return().throw("").value;
invariant(typeof yield_return_value !== "undefined");
invariant(typeof yield_return_value !== "number");
(yield_return_value : boolean); // error: catch (mixed) ~> boolean
