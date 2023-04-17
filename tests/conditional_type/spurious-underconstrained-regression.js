class C extends (1: any) {
  children: Array<C>;
}

declare function flat<T>(array: Array<T>): Array<T extends $ReadOnlyArray<infer E> ? E : T>

function foo(c: C): Array<C> {
  return flat(c.children.map(child => foo(child)).concat([c])); // no spurious underconstrained error.
}
