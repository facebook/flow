// @flow

// Sanity check:
// - predicate functions cannot have bodies (can only be declarations)

function pred(x: mixed): $StrP {
  return typeof x === "string"; // error: boolean incompatible with $StrP
}

function foo(x: string | Array<string>): string {

  if (pred(x)) {
    return x;
  }

  return "1"
}
