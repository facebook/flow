type Obj = {
  readonly foo: string,
};
type Valid = {
  readonly: string,
  readonly?: string,
  readonly(): string,
  readonly<T>(): T,
};

type Indexer = {
  readonly [string]: mixed;
};

class C {
  readonly prop: string;
}
class ValidC {
  readonly: void;
  readonly(): void {}
  readonly<T>(): T {}
  static readonly: void;
  static readonly(): void {}
  static readonly<T>(): T {}
}

interface I {
  readonly prop: string;
}
interface ValidI {
  readonly: string;
  readonly?: string;
  readonly(): T;
  readonly<T>(): T;
}

const readonly = 1;

type Tuple1 = [readonly foo: string];
type Tuple2 = [readonly a: string, readonly b: number];
type Tuple3 = [+a: string, readonly b: number, -c: boolean];
type Tuple4 = [readonly a?: string];
type TupleRegressionTest = [readonly: string]
