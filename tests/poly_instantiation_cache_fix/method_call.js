// @flow

declare class Set<T> {
  toArray(): T[];
  filter(predicate: (value: T) => unknown): Set<T>;
}

type S = Set<number> | Set<string>;

declare const set: S;
const arr = set.filter(_ => true).toArray();
arr as Array<number> | Array<string>;
