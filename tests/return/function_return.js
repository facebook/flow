class C {
    foo() { }
    bar() { return; }
    fn(x:number): number { return x; }
}

function f(x: number): number {
  if (x > 1) {
    return 42;
  }
}

function g(x: number): ?number {
  if (x > 1) {
    return 42;
  }
}

function h(x: number): number {
  if (x > 1) {
    return 42;
  }
  return;
}

function i(x: number): ?number {
  if (x > 1) {
    return 42;
  }
  return;
}

module.exports = C;

//function fn(x:number) { return x; }
//module.exports = fn;
