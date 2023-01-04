// @flow

declare function objectEntries<TKey, TValue>(obj: { +[TKey]: TValue}): Array<[TKey, TValue]>;
declare function groupBy<T>(array: $ReadOnlyArray<T>, keyFunc: T => string): {[key: string]: Array<T>, ...};

export type Obj = {
  a: ?number,
  b: ?number,
  c: ?number,
  d: ?number,
};

export function foo(originalRows: Array<Obj>): Array<Obj> {
  const groupedRows = groupBy(originalRows, (item: Obj) => (null: any));
  return objectEntries(groupedRows).map(([_, rows]) => ({
    a: rows.reduce((mr, row) => (row.a ?? 0) < (mr.a ?? 0) ? row : mr).a,
//                      ^
    b: rows.reduce((mr, row) => (row.b ?? 0) < (mr.b ?? 0) ? row : mr).b,
    c: rows.reduce((mr, row) => (row.c ?? 0) < (mr.c ?? 0) ? row : mr).c,
    d: rows.reduce((mr, row) => (row.d ?? 0) < (mr.d ?? 0) ? row : mr).d,
  }));
}
