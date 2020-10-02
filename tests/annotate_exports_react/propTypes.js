// @flow

const { PropTypes } = require('react');

const XShape = {
  x: PropTypes.number.isRequired
};
const FooShape = {
  foo: PropTypes.shape(XShape).isRequired
};

const Bar = PropTypes.shape(FooShape);

module.exports = {
  FooShape,
  Bar
};
