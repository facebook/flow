//@flow

declare function f<T>(): $Diff<T, {foo: number}>;

const a = f();
