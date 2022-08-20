// @flow

const React = require('react');
type PropsType = { }

class Child extends React.Component<PropsType> {}
const HocChild: React.ComponentType<PropsType> = (null: any);

class OkParent extends React.Component<void> {
  render = (): React.Node => <Child ref={this._handleChild} />;
  _handleChild = (child: ?Child): void => {};
}

class BadParent extends React.Component<void> {
  render = (): React.Node => <HocChild ref={this._handleChild} />;
  _handleChild = (child: ?HocChild): void => {}; // Error
}
