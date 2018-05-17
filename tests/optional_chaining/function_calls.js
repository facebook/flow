// @flow

type X = () => number;

declare var x1: ?X;
declare var x2: X;

(x1?.(): empty);
(x2?.(): empty)
