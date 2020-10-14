//@flow

declare var condition: number;

function f<X>(x: X): X {
  var y: X = x;
  var z: number = x;
  var w: number = y;
  if (condition === 0) {
    return 42;
  }
  return x;
}

function g<X: number>(x: X): X {
  var y: X = x;
  var z: number = x;
  var w: number = y;
  if (condition === 0) {
    return 42;
  }
  return x;
}

function overrideArgument<T>(arg: Array<T> | T): T {
  if (arg == null) {
    return arg;
  }

  if (Array.isArray(arg)) {
    return arg[arg.length - 1];
  }

  return arg;
}
