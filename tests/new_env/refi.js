//@flow

var x;

var z: unknown = 100;
z;
if (z) {
  x = 'a';
} else {
  x = 42;
}
x;

function f(): number {
  var y;
  if (typeof x === 'number') {
    y = x;
  } else if (z === 42) {
    y = z;
  } else {
    y = 100;
  }

  y as empty;

  return y;
}
