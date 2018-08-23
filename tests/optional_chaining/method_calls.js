// @flow

type X = {
  bar: ?(() => number),
};

declare var foo: ?X;

(foo?.bar?.(): empty);
(foo.bar?.(): empty);
(foo?.bar(): empty);
((foo?.bar)(): empty);
