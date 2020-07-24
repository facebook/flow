//@flow

const React = require("React");

export type Props = {
  /**
   * foo is an identifier property
   */
  foo: 1,
  /**
   * bar is a getter property
   */
  get bar(): 2,
  /**
   * baz is an identifier property with variance
   */
  +baz : 3,
  /**
   * qux is a literal property
   */
  'qux' : 4,
}

function FunctionComponent(x: Props) {}

class ClassComponent extends React.Component<Props> {}

class Foo {
  /**
   * this is a static property
   */
  static staticProp : 1;
  /**
   * this is a static method
   */
  static staticMethod() {}
  /**
   * this is an instance property
   */
  instanceProp : 2;
  /**
   * this is an instance method
   */
  instanceMethod() {}
  /**
   * property with variance
   */
  +varianceProp : 3;
}

module.exports = {FunctionComponent, ClassComponent, Foo}

let bar = {
  /**
   * identifier property of an object literal
   */
  a : 1,
  /**
   * literal property of an object literal
   */
  'b' : 2,
  /**
   * computed property of an object literal
   */
  ['c'] : 3,
  /**
   * annotated getter property of an object literal
   */
  get d(): number { return 4; },
  /**
   * unannotated getter property of an object literal
   */
  get e()      {},
  /**
   * method property of an object literal
   */
  f() {},
  /**
   * async method property of an object literal
   */
  async g() {},
}

bar.a;
//  ^
bar.b;
//  ^
bar.c;
//  ^
bar.d;
//  ^
bar.e;
//  TODO: investigate/fix get-def of unnanotated getter property
bar.f;
//  ^
bar.g;
//  ^
