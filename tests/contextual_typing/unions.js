//@flow

{
    const x: { ['A' | 'B' ]: (number) => number } = { [42]: (v) => v };
}
{
    const x: { ['A']: (number) => number } = { [42]: (v) => v };
}
{
    const x: { ... } | { ... } = { a: (v) => v };
}
{
    const x: { [number]: (number) => number } = { [42]: (v) => v };
}
{
    type StrCb = (string) => void;
    type NumCb = (number) => void;
    type Cb = $ReadOnly<{ cb?: ?(NumCb | StrCb) }>;
    declare var fn: (config: Cb) => {};
    fn({ cb: (x) => { (x: string | number); } }); // okay
    fn({ cb: (x) => { (x: string); } }); // error number ~> string
}
{
  declare var j: {
    mock(moduleFactory?: any): void
  }
  j.mock(() => ({ f: n => {} }));
}
{
  type A<T> = (T) => void;
  type B<T> = A<T> | A<number>;
  const x: B<string> = (x) => { (x: string); }; // error number ~> string
}
