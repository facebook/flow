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
  set d(a: mixed) {} // okay
  // statics are in a separate set
  static a = 1; // okay
  static b() {} // okay
  static get c() {} // okay
  static set d(a: mixed) {} // okay

  //
  // getters/setters
  //
  get getter() {} // okay
  set setter(a: mixed) {} // okay
}

class GettersAndSetters {
  get getterFirst1(): number { return 1 } // okay
  set getterFirst1(a: mixed) {} // okay as you can have one getter and one setter of the same name
  get getterFirst1(): number { return 1 } // error
  set getterFirst1(a: mixed) {} // error

  get getterFirst2(): number { return 1 } // okay
  get getterFirst2(): number { return 1 } // error
  set getterFirst2(a: mixed) {} // okay as you can have one getter and one setter of the same name
  set getterFirst2(a: mixed) {} // error

  set setterFirst1(a: mixed): number { return 1 } // okay
  get setterFirst1() {} // okay as you can have one getter and one setter of the same name
  set setterFirst1(a: mixed): number { return 1 } // error
  get setterFirst1() {} // error

  set setterFirst2(a: mixed): number { return 1 } // okay
  set setterFirst2(a: mixed): number { return 1 } // error
  get setterFirst2() {} // okay as you can have one getter and one setter of the same name
  get setterFirst2() {} // error

  propFirst = 1; // okay
  get propFirst(): number { return 1 } // error
  set propFirst(a: number) {} // error

  methodFirst() {} // okay
  get methodFirst(): number { return 1 } // error
  set methodFirst(a: mixed) {} // error
}

class Props {
  propFirst = 1; // okay
  propFirst = 2; // error

  methodFirst() {} // okay
  methodFirst = () => {}; // error

  get getterFirst(): number { return 1 } // okay
  getterFirst = 1; // error

  set setterFirst(a: number) { } // okay
  setterFirst = 1; // error
}

class Methods {
  methodFirst() {} // okay
  methodFirst() {} // error

  propFirst = () => {}; // okay
  propFirst() {} // error

  get getterFirst(): number { return 1 } // okay
  getterFirst() {} // error

  set setterFirst(a: mixed) { } // okay
  setterFirst() {}; // error
}

// validating the static error messages work as intended
class Statics {
  static a = () => {}; // okay
  static a = () => {}; // error
  static get a(): () => void { return () => {}; } // error
  static set a(a: mixed) { } // error
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
