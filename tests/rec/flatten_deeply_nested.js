type Flatten<T> = T extends ReadonlyArray<infer V> ? Flatten<V> : T; // no misplaced error
type DeepArray<+T> = T | ReadonlyArray<DeepArray<T>>;
type DeepArrayOfObject = DeepArray<{...}>;

const v: Flatten<DeepArrayOfObject> = {}; // ok
