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

module.exports = {FunctionComponent, ClassComponent}
