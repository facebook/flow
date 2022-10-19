// @flow

declare function idx<IdxObject, IdxResult>(object: IdxObject, f: (_: $Facebookism$IdxWrapper<IdxObject>) => IdxResult): ?$Facebookism$IdxUnwrapper<IdxResult>;

type Foo = {
  foo: ?{
    bar: string,
  }
};

function foo(x: ?Foo): ?string {
  return idx(x, _ => _.foo.);
//                         ^
}
