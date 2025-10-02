declare var foo: {
  @@dispose: void,
  @@asyncDispose: Promise<void>,
};

foo[Symbol.dispose] as void; // OK
foo[Symbol.asyncDispose] as Promise<void>; // OK
