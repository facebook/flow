// flowlint unnecessary-invariant:error

declare function assert(condition: unknown): asserts condition;

declare const obj: {};

// $FlowFixMe[unnecessary-invariant]
assert(obj); // suppressed by the former name

// $FlowFixMe[unnecessary-assertion]
assert(obj);

assert(obj); // error: reported as unnecessary-assertion
