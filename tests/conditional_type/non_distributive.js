function avoid_unnecessary_type_app_loop_regression_test() {
  type Obj = {+[string]: unknown};
  opaque type Opaque<+_V> = string;

  type Mapped<+O: Obj> = {
    +[Key in keyof O]: O[Key] extends Obj
      ? Opaque<O[Key]>
      : Opaque<O[Key]>,
  };

  declare const o: {+foo: '1'};
  (o: Mapped<{ foo?: unknown }>); // ok
  (o: Mapped<{[K in 'foo']?: unknown}>); // ok
}

function no_longer_always_distribute() {
  type Mapped<+O: {+foo: unknown, ...}> = O['foo'] extends string
    ? string
    : number

  declare const v: Mapped<{foo: string | number}>;
  (v: number); // ok
  (v: string); // error: number ~> string
}
