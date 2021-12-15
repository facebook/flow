//@flow

function exhaustive(x: 'a' | 'b'): number {
  switch (x) {
    case 'a': return 3;
    case 'b': return 3;
  }
}
