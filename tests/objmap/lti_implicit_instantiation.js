type F<T> = $ReadOnly<{
  log: (() => T) => void,
  ...
}>;

declare function map<O: {...}>(o1: O): $ObjMap<O, <V>(F<V>)=>V>;

const foo = Object.freeze({bar: {log: (f: () => string) => {}}})
const o1 = map(foo) as {bar: number}; // error
