// Regression test for `...args: infer _` rest-parameter inference in
// conditional types. A regular `infer` param (`F`/`V`) sitting alongside an
// inferred rest param must still resolve correctly. The infer tparams are
// pinned in reverse declaration order so that the rest tparam is solved first,
// which unblocks the multiflow that gives the regular param its real bound.
// All three of `a`, `b`, `c` should resolve to `string`.

interface Thenable<T> { then(cb: (value: T) => unknown): unknown }

// (a) Baseline, no rest params
type NoRest<T> = T extends interface { then(cb: infer F): unknown }
  ? F extends ((value: infer V) => unknown) ? V : 'NOT_CALLABLE' : 'NO_THEN';
declare const a: NoRest<Thenable<string>>;
a as string; // ok
a as empty; // error: string ~> empty

// (b) Rest in the matched method `then`
type RestInMethod<T> = T extends interface { then(cb: infer F, ...args: infer _): any }
  ? F extends ((value: infer V) => unknown) ? V : 'NOT_CALLABLE' : 'NO_THEN';
declare const b: RestInMethod<Thenable<string>>;
b as string; // ok
b as empty; // error: string ~> empty

// (c) Rest in the `extends` function type
type RestInFn<T> = T extends interface { then(cb: infer F): unknown }
  ? F extends ((value: infer V, ...args: infer _) => any) ? V : 'NOT_CALLABLE' : 'NO_THEN';
declare const c: RestInFn<Thenable<string>>;
c as string; // ok
c as empty; // error: string ~> empty
