/**
 * Regression test for annotation_inference AnnotSpecializeT match ordering fix.
 *
 * Tests that a class can extend another class's constructor property
 * (which resolves through FunProtoT) without producing a spurious
 * invalid-exported-annotation error.
 */

class Base {
  constructor() {}
  foo(): string {
    return "hello";
  }
}

const base: Base = new Base();

class Child extends base.constructor {
  bar(): string {
    return "world";
  }
}

module.exports = Child;
