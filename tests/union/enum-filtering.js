type Enum = 'ONE' | 'TWO';
type Selection = { x: 'ONE' } | { x: 'TWO' } | { x: 'NONE' }

type Rule = {
  x: Enum,
  y: Selection,
}

function foo(r: Rule) {
  const x = r.x;
  const y = r.y;
  if (y.x === x) {
    switch (y.x) {
      case 'ONE': break;
      case 'TWO': break;
      default: (y.x: empty); // We error here. Ideally we would not, so if you figure it out in the future with good perf, you can remove error from snapshot
    }
  }
}
