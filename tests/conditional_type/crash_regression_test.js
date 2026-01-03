// Should not crash!
type NonOptionalKeys<T> = Values<{
  [key in keyof T]: void extends T[key] ? empty : key,
}>;
type ObjectWithNonOptionalKey<T> = {
  [key in NonOptionalKeys<T>]: T[key],
};
type AnotherObject<TObject> = Readonly<{
  ...ObjectWithNonOptionalKey<{
    [key in keyof TObject]: TObject[key],
  }>,
}>;
