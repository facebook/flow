// @flow

import * as React from 'react';
import * as ReactDOM from 'react-dom';
import TestUtils from 'react-dom/test-utils';

class MyTestingComponent extends React.Component<{}> {
  _node: ?HTMLButtonElement;

  componentDidMount() {
    TestUtils.Simulate.click(this._node);
  }

  render() {
    return (
      <button className="my-button" ref={node => this._node = node} />
    );
  }
}

const tree = TestUtils.renderIntoDocument(<MyTestingComponent />);
TestUtils.mockComponent(MyTestingComponent);
TestUtils.mockComponent(MyTestingComponent, 'span');
(TestUtils.isElement(<MyTestingComponent />): boolean);
(TestUtils.isElementOfType(<MyTestingComponent />, MyTestingComponent): boolean);
const myButtonComponent = TestUtils.findRenderedDOMComponentWithClass(tree, 'my-button');
(TestUtils.isDOMComponent(myButtonComponent): boolean);
(TestUtils.isCompositeComponent(tree): boolean);
(TestUtils.isCompositeComponentWithType(tree, MyTestingComponent): boolean);
(TestUtils.findAllInRenderedTree(tree, child => child.tagName === 'BUTTON'): Array<React.Component>);
(TestUtils.scryRenderedDOMComponentsWithClass(tree, 'my-button'): Array<Element>);
(TestUtils.findRenderedDOMComponentsWithClass(tree, 'my-button'): ?Element);
(TestUtils.scryRenderedDOMComponentsWithTag(tree, 'button'): Array<Element>);
(TestUtils.findRenderedDOMComponentsWithTag(tree, 'button'): ?Element);
(TestUtils.scryRenderedComponentsWithType(tree, MyTestingComponent): Array<React.Component>);
(TestUtils.findRenderedComponentsWithType(tree, MyTestingComponent): ?React.Component);
