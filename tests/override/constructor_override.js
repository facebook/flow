// `override constructor()` is a parse-time error.

class Base {
  constructor() {}
}

class Sub extends Base {
  override constructor() { // ERROR: `override` cannot appear on a constructor
    super();
  }
}
