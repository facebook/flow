declare class MySet<+T> {}

type $ElementOfSet<SetType> = $Call<
  <ElementType>(MySet<ElementType>) => ElementType,
  SetType,
>;

class Entry<+X> {}

type SetOfEntries = MySet<Entry<mixed>>;

function test1() {
  type FilterEntry = $ElementOfSet<SetOfEntries>;

  declare var x: MySet<FilterEntry>;
  (x: SetOfEntries); // no error
}

function test2() {
  type FilterEntry = $ElementOfSet<SetOfEntries>;

  declare var y: SetOfEntries;
  (y: MySet<FilterEntry>); // no error
}

/**
 * This is a regression test for IntersectionT normalization.
 * Without the normalization, we will hit the issue of TypeAppExpansion being too conservative.
 */
function test3() {
  declare opaque type Opaque<-T>;

  type ID<T> = T;

  declare function wrapWithId<P1: {...}>(Opaque<P1>): Opaque<ID<P1>>;

  declare function spreadIt<P2: {...}>(
    Component: Opaque<P2>,
    spec: P2 => mixed
  ): Opaque<{...P2}>;

  // The HOCs above are basically convoluted identity functions...

  type Foo = {+foo: string};
  declare var OriginalComponent: Opaque<Foo>;

  const IdWrapped = wrapWithId(OriginalComponent);

  const Spreaded = spreadIt(
    IdWrapped,
    (p) => {},
  );

  (Spreaded: Opaque<{}>); // error: {} ~> Foo
}
