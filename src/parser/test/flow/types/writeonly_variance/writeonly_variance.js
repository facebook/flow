type Obj = {
  writeonly foo: string,
};
type Valid = {
  writeonly: string,
  writeonly?: string,
  writeonly(): string,
  writeonly<T>(): T,
};

type Indexer = {
  writeonly [string]: mixed;
};

class C {
  writeonly prop: string;
}
class ValidC {
  writeonly: void;
  writeonly(): void {}
  writeonly<T>(): T {}
  static writeonly: void;
  static writeonly(): void {}
  static writeonly<T>(): T {}
}

interface I {
  writeonly prop: string;
}
interface ValidI {
  writeonly: string;
  writeonly?: string;
  writeonly(): T;
  writeonly<T>(): T;
}

const writeonly = 1;

type Tuple1 = [writeonly foo: string];
type Tuple2 = [writeonly a: string, writeonly b: number];
type Tuple3 = [+a: string, writeonly b: number, -c: boolean];
type Tuple4 = [writeonly a?: string];
type TupleRegressionTest = [writeonly: string]

type ObjStringKey = {
  writeonly "foo": string,
};
type ObjNumberKey = {
  writeonly 0: string,
};
type ObjBigIntKey = {
  writeonly 0n: string,
};
