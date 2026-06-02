type A = {
  readonly f: ?{
    readonly id: ?string,
 }
};

type Foo = A['f'];

type B = {
  readonly f: ?{
    readonly id: ?string,
 }
};

type Bar = B['f'];

const _ = (arg: Foo | Bar) => {
  if (arg == null) {
    return null;
  }
  const id = arg.id;
}
