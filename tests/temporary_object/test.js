// @flow

const foo: $TEMPORARY$object<{| x?: number, y: number |}> = { y: 0 };

const bar: $TEMPORARY$object<{| [string]: number |}> = {foo: 3};
