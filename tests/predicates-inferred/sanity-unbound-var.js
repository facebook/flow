// @flow

declare var y: mixed;

// Sanity check: this should fail, because the preficate function
// checks `y` instead of `x`.
function $pred$err(x) {
  return typeof y === "string";
}

function foo(x: string | Array<string>): string {
  if ($pred$err(x)) {
    return x;
  } else {
    return x.join();
  }
}
