//@flow
type X<T> = Array<T> | X<Array<T>>;

[] as X<number>;

type Y<T> = ReadonlyArray<T> | Y<ReadonlyArray<T>>;

[] as Y<number>;

type Z<T> = [T] | Z<[T]>;

[] as Z<number>;
