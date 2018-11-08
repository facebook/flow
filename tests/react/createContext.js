// @flow

import React from 'react';

const Context = React.createContext('div');
const {Consumer, Provider} = Context;

class Foo extends React.Component<{}> {
  divRef: {current: null | HTMLDivElement} = React.createRef();

  render() {
    return (
      <React.Fragment>
        <Provider value='span'>
          <div ref={this.divRef}>
            <Consumer>
              {(Tag: 'div' | 'span' | 'img') => <Tag />}
            </Consumer>
          </div>
        </Provider>
        <Provider value='spam'> {/* Error: enum is incompatible with string */}
          <Consumer>
            {(Tag: 'div' | 'span' | 'img') => <Tag />}
          </Consumer>
        </Provider>
      </React.Fragment>
    );
  }

  componentDidMount() {
    var div: null | HTMLDivElement = this.divRef.current; // Ok
    var image: null | HTMLImageElement = this.divRef.current; // Error: HTMLDivElement is incompatible with HTMLImageElement
  }
}