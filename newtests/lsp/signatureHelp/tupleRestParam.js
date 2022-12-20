// @flow

declare function anonRest(...[string, number]): void;

anonRest(
  /* here */
);

declare function namedRest(...params: [string, number]): void;

namedRest(
  /* here */
);

declare function labeledTupleAnonRest(...[foo: string, bar: number]): void;

labeledTupleAnonRest(
  /* here */
);

declare function labeledTupleNamedRest(...params: [foo: string, bar: number]): void;

labeledTupleNamedRest(
  /* here */
);

declare function mixedTupleAnonRest(...[foo: string, number]): void;

mixedTupleAnonRest(
  /* here */
);

declare function mixedTupleNamedRest(...params: [foo: string, number]): void;

mixedTupleNamedRest(
  /* here */
);
