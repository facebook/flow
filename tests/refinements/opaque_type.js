export opaque type Opaque = string;

export type Dict = {
  [id: Opaque]: void,
};

export type Index = {
  index: Dict,
};

export type State = {
  o: null | Opaque,
  d: null | Index,
};

function test_refine_to_nullish() {
  declare opaque type Foo;
  declare const foo: ?Foo;

  // If we refine to null or void, regardless of the repr of opaque type,
  // it will always be refined to null or void
  if (foo == null) {
    foo as null | void | string; // ok
    foo as empty; // error
  }
  if (foo === undefined) {
    foo as void | string; // ok
    foo as empty; // error
  }
  if (foo === null) {
    foo as null | string; // ok
    foo as empty; // error
  }
}
