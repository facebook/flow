function object_test() {

  declare var x: unknown;

  const obj = {
    m(x: unknown): x is number {
      return typeof x === 'number';
    },
    f(x: unknown): x is string {
      return typeof x === 'string';
    },
    invalid(x: unknown): x is number {
      x = 1;
      return typeof x === 'number'; // error write reaches return
    },
  }

  if (obj.m(x)) {
    x as number;
    x as string; // error number ~> string
  }
  if (obj.f(x)) {
    x as number; // error string ~> number
    x as string;
  }

  // obj subtyping checks

  (obj as { // error number ~> string
    m(x: unknown): x is string,
    ...
  });
}
