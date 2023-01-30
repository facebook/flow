function foo1(x: Array<number>): [number, ?number] {
  return x; // Error, can't enforce arity when flowing array to tuple
}

function foo2(x: Array<number>): [number, ?number] {
  return [x[0], x[1]]; // OK. This is unsound, but at least arity is enforced
}

// Array literals with known elements can flow to tuples
{
  const arr = [1,2,3];
  (arr: [1,2,3]); // ok
  (arr: [1,2]); // error
  (arr: [1,2,3,4]); // error
}

// Fresh array -> tuple can subtype
([123]: [number | string]); // ok

// Array literals without known elements cannot flow to tuples
function foo3(arr: Array<number>): [number, number] { return arr; } // error

// Tuples cannot flow to arrays at the moment
function foo4(arr: [1,2]): Array<number> { return arr; } // error

// Destructuring tuple should eat the first few elements
{
  const tup: [1,2,3,4] = [1,2,3,4];
  const [a, b, ...rest] = tup;
  (a: 10); // error
  (b: 20); // error
  (rest: [3,40]); // error
}

// instanceof Array works for tuples
function foo5(tup: ?[number, number]): number {
  if (tup instanceof Array) {
    return tup[3]; // error
  } else {
    return 123;
  }
}
