/* @flow */
/*
---
id: react
title: React
permalink: /docs/react.html
prev: declarations.html
next: cli.html
---
*/

const React = require("react");

/*
  React applications are composed of nested components. As React-based
  applications grow, these component trees and the dependencies between them
  become increasingly complex.

  Flow's static analysis makes building large Web apps with React safe by
  tracking the types of props and state. Flow understands which props are
  required and also supports default props.

  Currently, Flow supports components defined using the `React.createClass`
  factory method and those defined using JavaScript classes inheriting from
  `React.Component`.

  Support for stateless functional components is coming soon.

  ## Defining components with the `createClass` factory

  ### PropTypes

  React ships with PropTypes, which verify the props provided to a component.
  Unlike the static analysis performed by Flow, PropTypes are only checked at
  runtime. If your testing doesn't trigger every code path that provides props
  to a component, you might not notice a type error in your program.

  Flow has built-in support for PropTypes. When Flow sees a `createClass`
  factory that specifies PropTypes, it is able to statically verify that all
  elements are constructed using valid props.
*/

const Greeter = React.createClass({
  propTypes: {
    name: React.PropTypes.string.isRequired,
  },
  render() {
    return <p>Hello, {this.props.name}!</p>;
  },
});

// $ExpectError
<Greeter />; // Missing `name`
// $ExpectError
<Greeter name={null} />; // `name` should be a string
<Greeter name="World" />; // "Hello, World!"

/*
  ### Default Props

  Flow understands when a default value is specified via `getDefaultProps` and
  will not error when the prop is not provided.

  Note that it's still a good idea to specify `isRequired`, even when a default
  value is provided, to protect against `null` prop values. React will only use
  a default value if the prop value is `undefined`.
*/

const DefaultGreeter = React.createClass({
  propTypes: {
    name: React.PropTypes.string.isRequired,
  },
  getDefaultProps() {
    return {name: "World"};
  },
  render() {
    return <p>Hello, {this.props.name}!</p>;
  },
});

<DefaultGreeter />; // "Hello, World!"
// $ExpectError
<DefaultGreeter name={null} />; // `name` should still be a string
<DefaultGreeter name="Flow" />; // "Hello, Flow!"

/*
  ### State

  Flow ensures that state reads and writes are consistent with the object
  returned from `getInitialState`.
*/

const Counter = React.createClass({
  getInitialState() {
    return {
      value: 0,
    };
  },
  increment() {
    this.setState({
      // $ExpectError
      value: this.state.value + "oops!",
    });
  },
  decrement() {
    // $ExpectError (todo: improve this error position)
    this.setState({
      valu: this.state.value - 1,
    });
  },
  render() {
    return (
      <div>
        <button onClick={this.increment}>+</button>
        <input type="text" size="2" value={this.state.value} />
        <button onClick={this.decrement}>-</button>
      </div>
    );
  },
});

/*
  ## Defining components as `React.Component` subclasses

  While PropTypes are great, they are quite limited. For example, it's possible
  to specify that a prop is some kind of function, but not what parameters that
  function accepts or what kind of value it might return.

  Flow has a much richer type system which is able to express those constraints
  and more. With class-based components, you can specify the your components'
  props, default props, and state using Flow's annotation syntax.
*/

class Button extends React.Component {
  props: {
    title: string,
    visited: boolean,
    onClick: () => void,
  };

  state: {
    display: 'static' | 'hover' | 'active';
  };

  static defaultProps = {
    visited: false,
  };

  onMouseEnter = () => this.setState({
    display: 'hover',
  });

  onMouseLeave = () => this.setState({
    display: 'static',
  });

  onMouseDown = () => this.setState({
    display: 'active',
  });

  constructor(props) {
    super(props);
    this.state = {
      display: 'static',
    };
  }

  render() {
    let className = 'button ' + this.state.display;
    if (this.props.visited) {
      className += ' visited';
    }

    return (
      <div className={className}
        onMouseEnter={this.onMouseEnter}
        onMouseLeave={this.onMouseLeave}
        onMouseDown={this.onMouseDown}
        onClick={this.props.onClick}>
        {this.props.title}
      </div>
    );
  }
}

function renderButton(container: HTMLElement, visited?: boolean) {
  const element = (
    <Button
      title="Click me!"
      visited={visited}
      onClick={() => {
        renderButton(container, true);
      }}
    />
  );
  React.render(element, container);
}

/*
  ## Higher-order components
*/

type Props<Config> = {
  class: ReactClass<Config>,
  config: Promise<Config>,
};

type State<Config> = {
  config: ?Config,
  loading: boolean,
};

class HOC<Config> extends React.Component<void, Props<Config>, State<Config>> {
  state: State<Config>;

  constructor(props) {
    super(props);
    this.state = {
      config: null,
      loading: false,
    };
  }

  load() {
    this.setState({loading: true});
    this.props.config.then(config => this.setState({
      loading: false,
      config
    }));
  }

  render() {
    if (this.state.config == null) {
      let label = this.state.loading ? "Loading..." : "Load";
      return (
        <button disabled={this.state.loading} onClick={this.load.bind(this)}>
          {label}
        </button>
      );
    } else {
      return React.createElement(
        this.props.class,
        this.state.config,
      );
    }
  }
}
