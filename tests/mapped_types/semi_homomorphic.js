type Semi<O: {...}, Keys: $Keys<O>> = {
  [key in Keys]: O[key],
}
{
  // Regular properties work
  type O = {+foo: number};

  type Mapped = Semi<O, 'foo'>;

  declare const a: Mapped;
  a.foo = 3; // ERROR

  type MappedBad = Semi<O, 'bar'>; // ERROR HERE
  declare const b: MappedBad;
  (b: {bar: empty}); // NO ERROR
}

{
  // Indexers work
  type P = {foo: number, bar: string, [string]: boolean};

  type MappedWithIndexer = Semi<P, 'foo' | string>;

  declare const withIndexer: MappedWithIndexer;
  (withIndexer: {foo: number, [string]: boolean}); // OK
  (withIndexer: {foo: number}); // ERROR, missing indexer

  type MappedWithoutIndexer = Semi<P, 'foo' | 'bar'>;

  declare const withoutIndexer: MappedWithoutIndexer;
  (withoutIndexer: {foo: number, bar: string}); // OK
  (withoutIndexer: P); // Arguably should error, but pre-existing unsoundness
  declare const p: P;
  (p: MappedWithoutIndexer); // ERROR

  // Incompatible indexers cause an error at the instantiation site
  // but persist the indexer type.
  type IndexedBad = Semi<P, number>; // ERROR
  declare const bad: IndexedBad;
  (bad: {[number]: boolean}); // NO ERROR

  // Indexers when the original type has none cause an error at the instantiation
  // site and an any-typed indexer
  type NoIndexerBad = Semi<{}, number>; // ERROR
  declare const noIndexerBad: NoIndexerBad;
  (noIndexerBad: {[number]: empty}); // NO ERROR
}
