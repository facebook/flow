type Tuple = [number, number];
type MappedTypeOnTuple = {[K in keyof Tuple]: Tuple[K] extends number ? string : empty};

// MappedTypeOnTuple should still work as a lower bound
function foo1(arr: MappedTypeOnTuple): [1, 2] {
  return arr; // error
}
// MappedTypeOnTuple should still work as a upper bound
function foo2(arr: [number, number]): MappedTypeOnTuple {
  return arr; // error
}

type F<T> = $ReadOnly<{
  log: (() => T) => void,
  ...
}>;

declare function map<A: $ReadOnlyArray<mixed>>(o2: Promise<A>): {[K in keyof A]: A[K] extends F<infer V> ? V : empty};
const boo = Promise.all([{log: (f: () => string) => {}}]);
map(boo) as [+v: number]; // error

declare const issue2674: {[K in keyof [mixed, mixed]]: 'FOO'};

(issue2674[0]: 'FOO'); // ok
(issue2674[0]: 'BAR'); // error
issue2674[0] = 'BAR'; // error
