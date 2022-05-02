/* @flow */

const React = require('react');
class Foo extends React.Component<{
  bar: string,
}> {}

const props = {bar: 42};
const blah = <Foo {...props} />; // error bar, number given string expected
