type O = {...empty, ...};
declare var o: O;
42 as O; // Error: number ~> empty
o as empty; // OK: empty ~> empty

function fn1<T>(x: {...T, ...}) {
  x as number; // Error: mixed ~> number,
               // but only one error. empty ~> number is ok.
}

function fn2<T>(fn: ({...T, ...}) => void) {
  fn({}); // Error: object ~> empty
}

function fn3<T>(x: T, fn: ({...T, foo: number, ...}) => void) {
  fn({...x, foo: 42}); // OK
}
