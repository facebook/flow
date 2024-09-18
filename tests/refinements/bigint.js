{
  declare const x: 1n | 2n;

  if (x === 1n) {
    x as 1n; // OK
  } else if (x === 2n) {
    x as 2n; // OK
  } else {
    x as empty; // OK
  }
}

// Negative
{
  declare const x: -1n | -2n;

  if (x === -1n) {
    x as -1n; // OK
  } else if (x === -2n) {
    x as -2n; // OK
  } else {
    x as empty; // OK
  }
}
