declare function assertTruthy(condition: unknown): asserts condition;
declare function assertGeneric<T>(condition: T): asserts condition;
declare function assertSecond(message: string, condition: unknown): asserts condition;
declare const ns: {assert(condition: unknown): asserts condition};

declare const yes: true;
declare const message: [string];

assertTruthy(yes); // constant-condition error
assertGeneric<true>(yes); // constant-condition error
ns.assert(yes); // constant-condition error
ns['assert'](yes);
assertSecond(...message, yes);
