type O = {| foo: number |}
type Box<T> = {contents: T};

type WithIndexer = {
  foo: number,
  [string]: string,
};


type Mapped<O: {...}> = {
  [key in keyof O]: Box<O[key]>,
};

// MappedType ~> ObjT
{
  declare const o: Mapped<O>;
  (o: {foo: {contents: number}}); // OK
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
}

// Optionality 
{
  type Partial<T: {...}> = {[key in keyof T]?: T[key]};
  declare const partial: Partial<O>;
  (partial.foo: number); // ERROR
  (partial.foo: number | void); // OK 

  declare const partialIndexer: Partial<WithIndexer>;
  (partialIndexer.qux: string); // ERROR
  (partialIndexer.qux: string | void); // OK 
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
