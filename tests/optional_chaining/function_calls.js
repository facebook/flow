// @flow

type X = () => number;

declare const x1: ?X;
declare const x2: X;

x1?.() as empty;
x2?.() as empty;

declare const x3: ?() => () => number;
x3?.()();
(x3?.())();
