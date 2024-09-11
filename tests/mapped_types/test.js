type O = {| foo: number |}
type Arr = Array<number>;
type ROArr = $ReadOnlyArray<number>;
type Tuple = [a: number, +b?: string];
type Box<T> = {contents: T};

type WithIndexer = {
  foo: number,
  [string]: string,
};


type Mapped<O: {...} | $ReadOnlyArray<mixed>> = {
  [key in keyof O]: Box<O[key]>,
};

// MappedType ~> ObjT
{
  declare const o: Mapped<O>;
  (o: {foo: {contents: number}}); // OK
}

// MappedType ~> array & tuple
{
  declare const a: Mapped<Arr>;
  declare const b: Mapped<ROArr>;
  declare const c: Mapped<Tuple>;
  (a: Array<{contents: number}>); // OK
  (b: $ReadOnlyArray<{contents: number}>); // OK
  (c: [a: {contents: number}, +b?: {contents: string | void}]); // OK
  (a: empty); // ERROR
  (b: empty); // ERROR
  (c: empty); // ERROR
}

// ObjT ~> MappedType
{
  declare const o: {foo: {contents: number}};
  (o: Mapped<O>); // OK

  declare const badKey: {bar: {contents: number}};
  (badKey: Mapped<O>); // ERROR

  declare const badVal: {foo: {contents: string}};
  (badVal: Mapped<O>); // ERROR
}

// array & tuple ~> MappedType
{
  declare const a: Array<{contents: number}>;
  declare const b: $ReadOnlyArray<{contents: number}>;
  declare const c: [a: {contents: number}, +b?: {contents: string | void}];
  declare const badA: Array<{contents: string}>;
  declare const badB: $ReadOnlyArray<{contents: string}>;
  declare const badC: [a: {contents: number}, +b: {contents: string}];
  (a: Mapped<Arr>); // OK
  (b: Mapped<ROArr>); // OK
  (c: Mapped<Tuple>); // OK
  (badA: Mapped<Arr>); // ERROR
  (badB: Mapped<ROArr>); // ERROR
  (badC: Mapped<Tuple>); // ERROR
}

// No mapped types in declared classes or interfaces
{
  declare class DeclaredClass {
    [x in keyof O]: number; // ERROR
  }
  interface I {
    [x in keyof O]: number; // ERROR
  }
}

// Mapped types with indexers
{
  type MappedIndexer = Mapped<WithIndexer>;
  declare var mappedIndexer: MappedIndexer;

  (mappedIndexer.foo: number); // ERROR
  (mappedIndexer.bar: string); // ERROR
  (mappedIndexer: {foo: Box<number>, [string]: Box<string>}); // OK

  declare var indexer: {foo: Box<number>, [string]: Box<string>};
  (indexer: MappedIndexer); // OK
}

// Variance
{
  type ReadOnly<T: {...}> = {+[key in keyof T]: T[key]};
  declare const readonly: ReadOnly<O>;
  (readonly.foo: number); // OK
  readonly.foo = 3; // ERROR

  type WriteOnly<T: {...}> = {-[key in keyof T]: T[key]};
  declare const writeonly: WriteOnly<O>;
  (writeonly.foo: number); // ERROR
  writeonly.foo = 3; // OK

  type ReadOnlyIndexer = ReadOnly<WithIndexer>;
  declare const readonlyIndexer: ReadOnlyIndexer;
  (readonlyIndexer.qux: string); // OK
  readonlyIndexer.qux = 'str'; // ERROR

  type _Unsupported = {+[key in keyof Arr]: Arr[key]}; // error: unsupported variance
}

// Optionality
{
  type Partial<T: {...} | $ReadOnlyArray<mixed>> = {[key in keyof T]?: T[key]};
  declare const partial: Partial<O>;
  (partial.foo: number); // ERROR
  (partial.foo: number | void); // OK

  declare const partialIndexer: Partial<WithIndexer>;
  (partialIndexer.qux: string); // ERROR
  (partialIndexer.qux: string | void); // OK

  declare const partialArr: Partial<Arr>;
  (partialArr[0]: number); // ERROR;
  declare const partialTuple: Partial<Tuple>;
  (partialTuple[0]: number); // ERROR;
}

// Error positioning
{
  type ConstrainedBox<T: string> = Box<T>;
  type MappedConstrained<O: {...}> = {
    [key in keyof O]: ConstrainedBox<O[key]>,
  };

  declare var constrained: MappedConstrained<O>; // ERROR HERE, NOT IN DEFINITION OF MAPPEDCONSTRAINED
  (constrained: {foo: {contents: number}}); // OK
}

// Error positioning
{
  type UnconstrainedKey<T> = {[key in T]: number};
  type BadKeys = UnconstrainedKey<boolean>; // ERROR HERE, NOT LINE ABOVE
  declare const badKeys: BadKeys;
  (badKeys: empty); // ERROR
}
