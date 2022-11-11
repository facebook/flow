// @flow

declare function id<T>(x:T): T;
declare function singletonArray<T>(T): Array<T>;

const arr1 = singletonArray("s"); // annotate
arr1.push(3);
const set1 = new Set(["a"]); // annotate
set1.add(3);

const arr2 = singletonArray("s"); // don't annotate
const set2 = new Set(["a"]); // don't annotate
