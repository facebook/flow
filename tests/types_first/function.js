export function foo() { return 1; } // missing annotation

export function functionWithTypeof(bar: string, baz: typeof bar, {boz}: {boz: typeof baz}) {}

export function functionWithTypeofAndDestructuring(
  {x: [x1], y: x2}: {x: Array<number>, y: string},
  y: typeof x1,
  z: typeof x2,
) {}

export function functionWithTypeofAndOptional(x?: number = 1, y: typeof x) {}
