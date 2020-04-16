// @flow

declare var cond: boolean;

declare function foo(): {
  f0: void,
  f1: void,
  f2: void,
  f3: void,
  f4: void,
  f5: void,
  f6: void,
  f7: void,
  f8: void,
  f9: void
};

// The expected annotation has size greater than 10 so we expect the type
// annotation to fail
module.exports = foo();
