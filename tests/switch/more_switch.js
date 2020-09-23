/* @flow */

function foo(x): number {
    switch (x) {
      case 0:
      case 1: return 1;
      default: throw new Error('hi');
    }
}

function bar(x) {
    switch (x) {
      case 0: break;
      default: return;
    }
    1;
}

function baz(x): number {
  switch (x) {
    case 0: break;
    case 1: return 1;
    default: throw new Error('hi');
  }
  return 2;
}

function boo(o: ?{x: 'a' | 'b'}): void {
  if (o != null) {
    switch (o.x) {
      case 'a':
        o = null;
        break;
      case 'b':
        break;
    }
  }
}
