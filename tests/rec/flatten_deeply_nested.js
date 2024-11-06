type Flatten<T> = T extends $ReadOnlyArray<infer V> ? Flatten<V> : T; // no misplaced error
type DeepArray<+T> = T | $ReadOnlyArray<DeepArray<T>>;
type DeepArrayOfObject = DeepArray<{}>;

const v: Flatten<DeepArrayOfObject> = {} // ok
