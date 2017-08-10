var React = require('react');

class C extends React.Component<void, {}> {
  foo(): number {
    return this.state.x; // error: need to declare type of state
  }
}
