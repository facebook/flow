// @flow

declare class Set<T> {
  toArray(): T[];
  filter(predicate: (value: T) => mixed): Set<T>;
}

type S = Set<number> | Set<string>;

declare var set: S;
const arr = set.filter(_ => true).toArray();
(arr: Array<number> | Array<string>);
