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
