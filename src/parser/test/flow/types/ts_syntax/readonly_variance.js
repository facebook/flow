type Obj = {
  readonly foo: string,
};
type Valid = {
  readonly: string,
};

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
