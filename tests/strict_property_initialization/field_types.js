type VoidableAlias = number | void;

class FieldsThatAcceptUndefined {
  anyValue: any;
  unknownValue: unknown;
  maybeValue: ?number;
  unionValue: number | void;
  voidValue: void;
  aliasValue: VoidableAlias;
}

class GenericField<T> {
  x: T; // error
}

class GenericVoidBound<T extends void> {
  x: T; // error: `T` may be `empty`, a narrower type than `void`
}
