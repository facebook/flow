// @flow

declare var t: [number, string];
declare var y: 0 | 1;
type T = $ElementType<typeof t, typeof y>;
type T1 = $ElementType<typeof t, 0 | 1>;
declare var v: T;
declare var v1: T1;
(v: number | string); // OK, correct
(v1: number | string);

(1: T); // works
(2: T1); // works
("foo": T); // works
("foo": T1); // works
({ a: 1 }: T); // error

declare var x: string & number;

(x: T); // works
(x: T1); // works

type Fields = {|
  a: string,
  b: number
|};

const data: Fields = {
  a: "foo",
  b: 42
};

function getField<K: $Keys<Fields>>(key: K): $ElementType<Fields, K> {
  return data[key];
}

const aValue = getField("a");
const bValue = getField("b");

