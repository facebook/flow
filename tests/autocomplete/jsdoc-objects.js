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
