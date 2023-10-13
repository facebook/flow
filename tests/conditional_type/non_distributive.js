function avoid_unnecessary_type_app_loop_regression_test() {
  type Obj = {+[string]: mixed};
  opaque type Opaque<+_V> = string;

  type Mapped<+O: Obj> = {
    +[Key in keyof O]: O[Key] extends Obj
      ? Opaque<O[Key]>
      : Opaque<O[Key]>,
  };

  declare const o: {+foo: '1'};
  (o: Mapped<{ foo?: mixed }>); // ok
  (o: Mapped<{[K in 'foo']?: mixed}>); // ok
}

function no_longer_always_distribute() {
  type Mapped<+O: {+foo: mixed, ...}> = O['foo'] extends string
    ? string
    : number

  declare const v: Mapped<{foo: string | number}>;
  (v: number); // ok
  (v: string); // error: number ~> string
}
