type Obj = {
  readonly foo: string,
};
type Valid = {
  readonly: string,
};

type Tuple = [readonly foo: 1];
type ValidTuple = [readonly: 1];

type Indexer = {
  readonly [string]: mixed;
};

class C {
  readonly prop: string;
}
class ValidC {
  readonly: string;
}

interface I {
  readonly prop: string;
}
interface ValidI {
  readonly: string;
}
