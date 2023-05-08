// This test checks ensures that we resolve TypeAppTs, KeysT, EvalTs, etc
// before trying to construct the mapped type.

type Mapped<T> = {[key in T]: number};

{
  type TypeApp<T> = T;
  type MappedTypeApp = Mapped<TypeApp<'foo'>>;
  declare const mappedTypeApp: MappedTypeApp;
  (mappedTypeApp: empty); // ERROR
  (mappedTypeApp: {foo: number}); // OK!
}

{
  type Keys = $Keys<{foo: number}>;
  type MappedKeys = Mapped<Keys>;
  declare const mappedKeys: MappedKeys;
  (mappedKeys: empty); // ERROR
  (mappedKeys: {foo: number}); // TODO: OK, error for now
}

{
  type O = {foo: 'foo'};
  type Eval = O['foo'];
  type MappedEval = Mapped<Eval>;
  declare const mappedEval: MappedEval;
  (mappedEval: empty); // ERROR
  (mappedEval: {foo: number});
}
