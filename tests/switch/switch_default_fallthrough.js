/**
 * @flow
 */
function foo(x: mixed): string {
  var a: number | string = '';
  var b: number | string = '';

  switch (x) {
    case 'foo':
      a = 0;
    default:
      b = 0;
  }

  // a is now string | number
  a as string; // error, string | number ~/> string
  a as number; // error, string | number ~/> number

  // b is now number
  b as number; // ok
  return b; // error, number ~/> string
}

function baz(x: mixed): number {
  var a: number | string = '';
  var b: number | string = '';

  switch (x) {
    case 'baz':
      a = 0;
      break;
    case 'bar':
      a = '';
    default:
      b = 0;
  }

  // a is now string | number
  a as string; // error, string | number ~/> string
  a as number; // error, string | number ~/> number

  // b is now string | number
  b as string; // error, string | number ~/> string
  b as number; // error, string | number ~/> number

  return a + b; // error, string ~/> number
}
