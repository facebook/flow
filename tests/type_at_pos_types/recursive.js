// @flow

var o = { m() { return this; } };
o.m();

type AList = ?{data: number, next: AList};
function alist(n: number): AList {
  if (n <= 0) return null;
  else return { data: n, next: alist(n - 1) };
}

const a = alist(10);

type BList = {data: number, a_next: AList, next: BList} | null | '';
function blist(n: number): BList {
  if (n <= 0) return null;
  if (n > 1)  return "";
  return {
    data: n,
    a_next: alist(n - 1),
    next: blist(n-1),
  };
}

type CList = {data: number, a_next: AList, next: BList, next: CList} | null | '';
function clist(n: number): CList {
  if (n <= 0) return null;
  if (n > 1) return "";
  return {
    data: n,
    a_next: alist(n - 1),
    b_next: blist(n - 1),
    next: clist(n - 1),
  };
}

// The following exhibits a use of the RemoveTopLevelTvarVisitor
//
// mu X . ((X | string) | string)
// ==>
// mu X . ((Bot | string) | string)
// ==>
// mu X . (string | string)
// ==>
// mu X . string
// ==>
// string
let x = "";
while (0 < 1) {
  x = (0 < 1) ? x : "";
}
x;

type CA<+T> = $ReadOnlyArray<CA<T>>;
