/**
 * @flow
 */

class ClassFoo4 {
  returnsANumber(): number { return 42; }
}

class ClassFoo5 {}

function givesAFoo4(): ClassFoo4 {
  return new ClassFoo4();
}

function givesAFoo5(): ClassFoo5 {
  return new ClassFoo5();
}

exports.ClassFoo4 = ClassFoo4;
exports.ClassFoo5 = ClassFoo5
exports.givesAFoo4 = givesAFoo4;
exports.givesAFoo5 = givesAFoo5;
