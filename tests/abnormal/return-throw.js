declare function invariant(boolean): void;

function f1(b: boolean): number {
  if (b) {
    return 1;
  } else {
    throw 'error';
  }
  'dead code'; // ERROR
}

function f2(b: boolean): number {
  if (b) {
    return 1;
  } else {
    invariant(false);
  }
  'dead code'; // ERROR
}

function f3(n: number): number {
  switch (n) {
    case 1:
      return 1;
    default:
      throw 'error';
  }
  'dead code'; // ERROR
}

function f4(n: number): number {
  switch (n) {
    case 1:
      return 1;
    default:
      invariant(false);
  }
  'dead code'; // ERROR
}
