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

/**
 * a defaulted string enum
 */
enum DefaultedStringEnum {
  /**
   * first member of a defaulted string enum
   */
  Member1,
  /**
   * second member of a defaulted string enum
   */
  Member2,
}

/**
 * an initialized string enum
 */
enum InitializedStringEnum {
  /**
   * first member of an initialized string enum
   */
  Member1 = '?',
  /**
   * second member of an initialized string enum
   */
  Member2 = '!',
}

/**
 * a number enum
 */
enum NumberEnum {
  /**
   * first member of a number enum
   */
  Member1 = 15,
  /**
   * second member of a number enum
   */
  Member2 = 150,
}

/**
 * a boolean enum
 */
enum BooleanEnum {
  /**
   * first member of a boolean enum
   */
  Member1 = true,
  /**
   * second member of a boolean enum
   */
  Member2 = false,
}

/**
 * a symbol enum
 */
enum SymbolEnum of symbol {
  /**
   * first member of a boolean enum
   */
  Member1,
  /**
   * second member of a boolean enum
   */
  Member2,
}

module.exports = {
  FunctionComponent,
  ClassComponent,
  Foo,
  DefaultedStringEnum,
  InitializedStringEnum,
  NumberEnum,
  BooleanEnum,
  SymbolEnum,
}
