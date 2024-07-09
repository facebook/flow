{
  declare const x: ['a', number] | ['b', string];

  if (x[0] === 'a') {
    x[1] as number; // OK
    x[1] as empty; // ERROR
  } else {
    x[1] as string; // OK
  }
}

{
  declare const x: ['a', number] | [null, string];

  if (x[0] != null) {
    x[1] as number; // OK
    x[1] as empty; // ERROR
  } else {
    x[1] as string; // OK
  }
}

{
  declare const x: [true, number] | [false, string];

  if (x[0]) {
    x[1] as number; // OK
    x[1] as empty; // ERROR
  } else {
    x[1] as string; // OK
  }
}

// End-to-end example using function args
function calc(...args: ['abs', number] | ['add', number, number]): number {
  if (args[0] === 'abs') {
    return Math.abs(args[1]);
  } else {
    return args[1] + args[2];
  }
}

calc('abs', -2); // OK
calc('abs', -2, 1); // ERROR

calc('add', 3, 4); // OK
calc('add', 999); // ERROR

// Element exists check
{
  declare const x: [true, number] | [false, string, true];
  if (x[2]) { // Unlike for objects, we error here. We can consider not in the future.
    x as empty; // ERROR
    x as [false, string, true]; // OK
  } else {
    x as empty; // ERROR
    x as [true, number]; // OK
  }
}

{
  // Non-readable
  declare const x: [true, number] | [false, string, -c: true];
  if (x[2]) { // ERROR: element not readable
    x as empty; // ERROR
  }
}
