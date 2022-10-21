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

const math1 = {
  foo: "Hello",
  subobj: {
    foo: is_zero,
    meth(x: number): bool { return true }
  },
  odd(x: number): bool {
    if (x === 0) {
      return false;
    }
    return math1.even(x - 1)
  },
  even: (x: number): bool => {
    if (x === 0) {
      return true;
    }
    return math1.odd(x - 1)
  }
}

const math2 = {
  foo: "Hello",
  subobj: {
    foo: math1,
    meth(x: number) { return true }
  },
  odd(x: number) {
    if (x === 0) {
      return false;
    }
    return math2.even(x - 1)
  },
  even: (x: number) => {
    if (x === 0) {
      return true;
    }
    return math2.odd(x - 1)
  }
}

const odd_obj = {
  unrelated() { return 42 },
  odd(x: number) {
    if (x === 0) {
      return false;
    }
    return even_obj.even(x - 1)
  }
}

const even_obj = {
    even: (x: number) => {
    if (x === 0) {
      return true;
    }
    return odd_obj.odd(x - 1)
  }
}

const recursiveArrow = () => true ? 1 : recursiveArrow();
