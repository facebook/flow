declare var foo: {
  @@iterator: Iterator<number>,
};

foo[Symbol.iterator] as Iterator<number>; // OK
foo[Symbol.iterator] as boolean; // Error
