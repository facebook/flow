declare var foo: {
  @@iterator: Iterator<number>,
};

(foo[Symbol.iterator]: Iterator<number>); // OK
(foo[Symbol.iterator]: boolean); // Error
