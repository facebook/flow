// @flow

type A = $Call<() => number>;
type B = $ObjMap<{}, () => number>;
type C = $ObjMapi<{}, () => number>;
type D = $ObjMapConst<{}, number>;
