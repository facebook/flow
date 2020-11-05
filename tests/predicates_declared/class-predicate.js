// @flow

class Hello {
  test(x: mixed): boolean %checks {
    return typeof x === "string";
  }
}

const m = new Hello();
const foo: string | number = "";
if (m.test(foo)) {
  (foo: string);
}
