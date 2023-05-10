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
