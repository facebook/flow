//@flow


class TimeOfDay {
  a(other: TimeOfDay): boolean {
    return true
  }
}

function f(): number {
  return f();
}

function f_na() {
  if (true) {
    return 42;
  }
  return f_na();
}

function havoc_x() {
  x = null;
}

var x;
havoc_x();
x = x;

function odd(x: number) {
  if (x === 0) {
    return false;
  }
  return even(x - 1)
}

function even(x: number) {
  if (x === 0) {
    return true;
  }
  return odd(x - 1)
}

function is_zero(x: number): boolean %checks {
  return x === 0 || is_zero(x);
}
