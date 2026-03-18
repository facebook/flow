// Regression test for FlowSet::clear() destroying speculation levels.
// The fix ensures move_top_entries_to_level guards against target indices
// that exceed the level count after clear() empties the stack.
//
// This exercises the code paths involved: generic function calls trigger
// implicit instantiation (which pushes preserve_level on the constraint
// cache), and union-typed arguments trigger speculation (which pushes/pops
// levels). The clear() call at SCC resolution boundaries could destroy
// the levels that speculation expects to find.
//
// Note: The actual panic required specific timing of SCC resolution
// boundaries + speculation in large codebases. This test exercises the
// relevant code paths to guard against regressions.

declare function identity<T>(x: T): T;
declare function choose<T>(x: T, y: T): T;
declare function merge<A, B>(a: A, b: B): A & B;

// Union types to trigger speculation during implicit instantiation
type Status = 'active' | 'inactive';

function processStatus(s: Status): string {
  return choose(s, 'active');
}

// Generic class with methods that involve speculation
class Container<T> {
  value: T;
  constructor(value: T) {
    this.value = value;
  }
  get(): T {
    return identity(this.value);
  }
}

// Mutually referencing types to create SCC boundaries
type Node = {
  value: string,
  next: ?Edge,
};

type Edge = {
  target: Node,
  label: Status,
};

function buildGraph(label: Status): Node {
  const node: Node = {
    value: choose("a", "b"),
    children: [], // ERROR: property not in Node
  };
  return node;
}

// Multiple generic calls with unions across definitions
const c = new Container<Status>('active');
const s = c.get();
const result = choose(s, 'inactive');
const merged = merge({a: 1}, {b: "two"});
