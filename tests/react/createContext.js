import * as React from 'react';

{
  const Context = React.createContext<string>('div');
  const {Consumer, Provider} = Context;

  class Foo extends React.Component<{}> {
    divRef: {current: null | DivInstance} = React.createRef();

    render(): React.Node {
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
      var div: null | DivInstance = this.divRef.current; // Ok
      var image: null | ImgInstance = this.divRef.current; // Error: DivInstance is incompatible with ImgInstance
    }
  }
}

{
  const Context = React.createContext(
    {foo: 0, bar: 0, baz: 0},
    (a, b) => {
      let result = 0;
      if (a.foo !== b.foo) {
        result |= 0b001;
      }
      if (a.bar !== b.bar) {
        result |= 0b010;
      }
      return result;
    },
  );
}

{
  const ThemeContext = createContext("light");
  ThemeContext.displayName = "ThemeContext";
}
