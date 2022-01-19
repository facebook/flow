// @flow

class Base {
  base_prop = 1; // okay
  base_method() {} // okay
}

class OkayCases extends Base {
  //
  // overriding inherited members should be allowed
  //
  base_prop = 2; // okay
  base_method() {} // okay

  //
  // static members should not collide with instance members
  //
  a = 1; // okay
  b() {} // okay
  get c() {} // okay
  set d(a) {} // okay
  // statics are in a separate set
  static a = 1; // okay
  static b() {} // okay
  static get c() {} // okay
  static set d(a) {} // okay

  //
  // getters/setters
  //
  get getter() {} // okay
  set setter(a) {} // okay
}

class GettersAndSetters {
  get getterFirst1() { return 1 } // okay
  set getterFirst1(a) {} // okay as you can have one getter and one setter of the same name
  get getterFirst1() { return 1 } // error
  set getterFirst1(a) {} // error

  get getterFirst2() { return 1 } // okay
  get getterFirst2() { return 1 } // error
  set getterFirst2(a) {} // okay as you can have one getter and one setter of the same name
  set getterFirst2(a) {} // error

  set setterFirst1(a) { return 1 } // okay
  get setterFirst1() {} // okay as you can have one getter and one setter of the same name
  set setterFirst1(a) { return 1 } // error
  get setterFirst1() {} // error

  set setterFirst2(a) { return 1 } // okay
  set setterFirst2(a) { return 1 } // error
  get setterFirst2() {} // okay as you can have one getter and one setter of the same name
  get setterFirst2() {} // error

  propFirst = 1; // okay
  get propFirst() { return 1 } // error
  set propFirst(a) {} // error

  methodFirst() {} // okay
  get methodFirst() { return 1 } // error
  set methodFirst(a) {} // error
}

class Props {
  propFirst = 1; // okay
  propFirst = 2; // error

  methodFirst() {} // okay
  methodFirst = () => {}; // error

  get getterFirst() { return 1 } // okay
  getterFirst = 1; // error

  set setterFirst(a) { } // okay
  setterFirst = 1; // error
}

class Methods {
  methodFirst() {} // okay
  methodFirst() {} // error

  propFirst = () => {}; // okay
  propFirst() {} // error

  get getterFirst() { return 1 } // okay
  getterFirst() {} // error

  set setterFirst(a) { } // okay
  setterFirst() {}; // error
}

// validating the static error messages work as intended
class Statics {
  static a = () => {}; // okay
  static a = () => {}; // error
  static get a() { return () => {}; } // error
  static set a(a) { } // error
  static a() {} // error
}

// validating that declared members are covered as well
class Declared {
  declare a: string; // okay
  declare a: string; // error

  b = ''; // okay
  declare b: string; // error

  c() {} // okay
  declare c: () => void; // error

  get d() {} // okay
  declare d: void; // error
}
