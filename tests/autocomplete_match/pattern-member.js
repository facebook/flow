declare const x: number;

declare const O: {foo: 1, bar: {baz: 2}, 'zip-zap': 3};

const e = match (x) {
   O. : 0,
//   ^
   O.f : 0,
//    ^
   O.bar.b : 0,
//        ^
   O[ ]: 0,
//   ^
};
