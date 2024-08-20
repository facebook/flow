/**
 * @flow
 */

const React = require('react');

type Props = {
  elements: Array<React.MixedElement>,
};

type State = {
};

class Foo extends React.Component<Props, State> {
  bar() {
    const x = this.props.elements;
    let sliceFrom = x;
    while (sliceFrom.length < 0) {
      sliceFrom = sliceFrom.concat(sliceFrom);
    }
  }
}
