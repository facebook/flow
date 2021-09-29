/* @flow */

declare function invariant(): empty; // raises

function foo1(c: bool): string {
  const y = c ? 5 : invariant();
  return "default string";
}


function foo2(c: bool): string {
  c ? 5 : invariant(false);
  return "default string";
}


function foo3(c: bool): string {
  const y = c ? invariant() : invariant(false);
  return "default string"; // Error: unreachable
}


function foo4(c: bool): string {
  const y = false ? 5 : invariant(false);
  return "default string";
}


function foo5(c: bool): string {
  invariant()
  return "default string"; // Error: unreachable
}


function foo6(c: bool): string {
  invariant(false)
  return "default string"; // Error: unreachable
}

function foo7(c: bool): string {
  invariant(c)
  return "default string";
}

function foo8(c: bool): string {
  return c ? 'a' : invariant();
}

function foo9(c: bool): string {
  return c ? 1 : invariant(); // Error: number is incompatible with string
}


function foo10(c: bool): string {
  return c ? invariant() : invariant();
}

function foo11(): string {
  return invariant() ? 1 : 2;
}

// `||`
function foo12(c: bool): string {
  c || invariant();
  return "default string";
}

function foo13(c: bool): string {
  c || invariant(false);
  return "default string";
}

function foo14(c: bool): string {
  invariant() || c;
  return "default string"; // Error: unreachable
}

function foo15(c: bool): string {
  return c || invariant(); // Error: return incompatible with string
}

function foo16(c: bool): string {
  return invariant() || invariant();
}

// `&&`
function foo17(c: bool): string {
  c && invariant();
  return "default string";
}

function foo18(c: bool): string {
  c && invariant(false);
  return "default string";
}

function foo19(c: bool): string {
  invariant() && c;
  return "default string"; // Error: unreachable
}

function foo20(c: bool): string {
  return c && invariant(); // Error: return incompatible with string
}

function foo21(c: bool): string {
  return invariant() && invariant();
}

// `??`
function foo22(c: bool): string {
  c ?? invariant();
  return "default string";
}

function foo23(c: bool): string {
  c ?? invariant(false);
  return "default string";
}

function foo24(c: bool): string {
  invariant() ?? c;
  return "default string"; // Error: unreachable
}

function foo25(c: ?bool): string {
  return c ?? invariant(); // Error: return incompatible with string
}

function foo26(c: ?string): string {
  return c ?? invariant(); // OK - either `c` is `string` or we throw
}

function foo27(c: bool): string {
  return invariant() && invariant();
}
