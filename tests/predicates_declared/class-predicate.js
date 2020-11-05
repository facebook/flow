// @flow

class Hello {
  test(x: mixed): boolean %checks {
    return typeof x === "string";
  }
}

const m = new Hello();
const foo: string | number = "";
if (m.test(foo)) {
  (foo: string); // okay
}

class D1 extends Hello {
  test(x: mixed): boolean %checks { // okay
    return typeof x === "string";
  }
}

class D2 extends Hello {
  test(x: mixed): boolean %checks { // error incompatible predicates
    return typeof x === "number";
  }
}

class D3 extends Hello {
  test(x: mixed): boolean { // error non-predicate method
    return typeof x === "string";
  }
}
