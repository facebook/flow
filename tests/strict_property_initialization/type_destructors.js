class IndexedMissing<O extends {readonly label: ?string}> {
  value: O['label']; // known gap: TypeScript reports this field
}

class IndexedWithVoid<O extends {readonly label: ?string}> {
  value: O['label'] | void;
}

type VoidableObject = {readonly label: ?string};

class ConcreteIndexed {
  value: VoidableObject['label'];
}
