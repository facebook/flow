/* @flow */

declare function invariant(): empty; // raises

function foo(c: bool): string {
  const y = c ? 5 : invariant();
  return "default string";
}


function foo(c: bool): string {
  c ? 5 : invariant(false);
  return "default string";
}


function foo(c: bool): string {
  const y = c ? invariant() : invariant(false);
  return "default string"; // Error: unreachable
}


function foo(c: bool): string {
  const y = false ? 5 : invariant(false);
  return "default string";
}


function foo(c: bool): string {
  invariant()
  return "default string"; // Error: unreachable
}


function foo(c: bool): string {
  invariant(false)
  return "default string"; // Error: unreachable
}

function foo(c: bool): string {
  invariant(c)
  return "default string";
}

function foo(c: bool): string {
  return c ? 'a' : invariant();
}

function foo(c: bool): string {
  return c ? 1 : invariant(); // Error: number is incompatible with string
}


function foo(c: bool): string {
  return c ? invariant() : invariant();
}

function foo(): string {
  return invariant() ? 1 : 2;
}

// `||`
function foo(c: bool): string {
  c || invariant();
  return "default string";
}

function foo(c: bool): string {
  c || invariant(false);
  return "default string";
}

function foo(c: bool): string {
  invariant() || c;
  return "default string"; // Error: unreachable
}

function foo(c: bool): string {
  return c || invariant(); // Error: return incompatible with string
}

function foo(c: bool): string {
  return invariant() || invariant();
}

// `&&`
function foo(c: bool): string {
  c && invariant();
  return "default string";
}

function foo(c: bool): string {
  c && invariant(false);
  return "default string";
}

function foo(c: bool): string {
  invariant() && c;
  return "default string"; // Error: unreachable
}

function foo(c: bool): string {
  return c && invariant(); // Error: return incompatible with string
}

function foo(c: bool): string {
  return invariant() && invariant();
}

// `??`
function foo(c: bool): string {
  c ?? invariant();
  return "default string";
}

function foo(c: bool): string {
  c ?? invariant(false);
  return "default string";
}

function foo(c: bool): string {
  invariant() ?? c;
  return "default string"; // Error: unreachable
}

function foo(c: ?bool): string {
  return c ?? invariant(); // Error: return incompatible with string
}

function foo(c: ?string): string {
  return c ?? invariant(); // OK - either `c` is `string` or we throw
}

function foo(c: bool): string {
  return invariant() && invariant();
}
