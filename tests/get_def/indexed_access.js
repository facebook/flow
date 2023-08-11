type T = {
  foo: string,
}

type U = T['foo'];
//          ^

type V = T['foo'];
//       ^
