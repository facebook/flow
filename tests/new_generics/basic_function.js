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
