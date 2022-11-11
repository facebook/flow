// @flow

type T = {type: 1, foo: 3} | {type: 2, bar: "4"};
const arrMap = [].map(_ => { // annotate
  return {type: 1, foo: 3};
});
(arrMap: Array<T>);

const arrReduce = [].reduce(acc => acc, {type: 1, foo: 3}); // don't annotate
(arrReduce: T);
