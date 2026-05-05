type Obj = {
  readonly foo: string, // OK
};
type Valid = {
  readonly: string, // OK
  readonly?: string, // OK
  readonly<T>(number): string, // OK
};

type Indexer = {
  readonly [string]: unknown; // OK
};

class C {
  readonly prop: string; // OK
}

interface I {
  readonly prop: string; // OK
}

const readonly = 1; // OK

type T = [readonly foo: number]; // OK

type ObjStringKey = {
  readonly "foo": string, // OK
};
type ObjNumberKey = {
  readonly 0: string, // OK
};
