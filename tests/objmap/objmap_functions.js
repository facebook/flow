// @flow

type OriginalFuncs = {
  funcA: (a: string) => number,
  funcB: (b: number) => string
};

type WrapInPromise = <V>((arg: any) => V) => Promise<V>;

type FuncsAsPromises = $ObjMap<OriginalFuncs, WrapInPromise>;

const ExpectedFuncs = {
  funcA: (a: string) => Promise.resolve(1),
  funcB: (a: number) => Promise.resolve('a'),
};

(ExpectedFuncs: FuncsAsPromises); // ok
