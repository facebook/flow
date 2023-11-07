//@flow
type X<T> = Array<T> | X<Array<T>>;

[] as X<number>;

type Y<T> = $ReadOnlyArray<T> | Y<$ReadOnlyArray<T>>;

[] as Y<number>;

type Z<T> = [T] | Z<[T]>;

[] as Z<number>;
