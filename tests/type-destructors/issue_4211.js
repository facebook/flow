// @flow

declare var t: [number, string];
type T = $ElementType<typeof t, 0 | 1>;
declare var v: T;
(v: number | string); // OK, correct

('foo': T); // works

declare var x: string & number;

(x: T); // works
