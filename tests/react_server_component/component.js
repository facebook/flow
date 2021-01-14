//@flow
const React = require('react');

type Props = {|
  num: number,
  exactObject: {| foo: number |},
  inexactObject?: {...},
  fn?: () => void,
|}

module.exports = function (props: Props): React.Node { return null }
