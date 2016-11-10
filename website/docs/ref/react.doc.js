/* @flow */
/*
---
id: react
title: React
permalink: /docs/react.html
prev: utility-types.html
---
*/

const React = require("react");
const ReactDOM = require("react-dom");

/*
  React applications are composed of nested components. As React-based
  applications grow, these component trees and the dependencies between them
  become increasingly complex.

  Flow's static analysis makes building large Web apps with React safe by
  tracking the types of props and state. Flow understands which props are
  required and also supports default props.

  React provides a few different ways to define components:

  * [the `React.createClass` factory](#the-reactcreateclass-factory),
  * [`React.Component` subclasses](#defining-components-as-reactcomponent-subclasses),
  * and [stateless functional components](#stateless-functional-components)

  This document explains how to make strongly-typed components using all of the
  above styles and includes an example of a [higher order
  component](#higher-order-components).
*/

/*
  ## The `React.createClass` factory

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
    // Note: Typo below is intentional
    // $ExpectError(todo: improve this error position)
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
  and more. With class-based components, you can specify the components'
  props, default props, and state using Flow's annotation syntax.
*/
type Props = {
  title: string,
  visited: boolean,
  onClick: () => void,
};

class Button extends React.Component {
  props: Props;

  state: {
    display: 'static' | 'hover' | 'active';
  };

  static defaultProps: { visited: boolean };

  onMouseEnter: () => void;
  onMouseLeave: () => void;
  onMouseDown: () => void;

  constructor(props: Props) {
    super(props);
    this.state = {
      display: 'static',
    };

    const setDisplay = display => this.setState({display});

    this.onMouseEnter = () => setDisplay('hover');
    this.onMouseLeave = () => setDisplay('static');
    this.onMouseDown = () => setDisplay('active');
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
Button.defaultProps = { visited: false };

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
  ReactDOM.render(element, container);
}

/*
  ## Stateless functional components

  Any function that returns a React element can be used as a component class in
  a JSX expression.
*/

function SayAgain(props: { message: string }) {
  return (
    <div>
      <p>{props.message}</p>
      <p>{props.message}</p>
    </div>
  );
}

<SayAgain message="Echo!" />;
<SayAgain message="Save the whales!" />;

/*
  Stateless functional components can specify default props as destructuring
  with default values.
*/

function Echo({ message, times = 2 }: { message: string, times?: number }) {
  var messages = new Array(times).fill(<p>{message}</p>);

  return (
    <div>
      {messages}
    </div>
  );
}

<Echo message="Helloooo!" />;
<Echo message="Flow rocks!" times={42} />;

/*
  ## Higher-order components

  Occasionally, repeated patterns in React components can be abstracted into
  functions that transform one component class into another.

  In the example below, the HOC `loadAsync` takes a component that requires some
  arbitrary config and a promise that will eventually provide that config and
  returns a component that takes care of loading the data and displaying the
  component asynchronously.
*/

function loadAsync<Config>(
  ComposedComponent: ReactClass<Config>,
  configPromise: Promise<Config>,
): ReactClass<{}> {
  return class extends React.Component {
    state: {
      config: ?Config,
      loading: boolean,
    };

    load: () => void;

    constructor(props) {
      super(props);

      this.state = {
        config: null,
        loading: false,
      }

      this.load = () => {
        this.setState({loading: true});
        configPromise.then(config => this.setState({
          loading: false,
          config,
        }));
      }
    }

    render() {
      if (this.state.config == null) {
        let label = this.state.loading ? "Loading..." : "Load";
        return (
          <button disabled={this.state.loading} onClick={this.load}>
            {label}
          </button>
        );
      } else {
        return <ComposedComponent {...this.state.config} />
      }
    }
  }
}

const AsyncGreeter = loadAsync(Greeter, new Promise((resolve, reject) => {
  setTimeout(() => {
    resolve({ name: "Async World" });
  }, 1000);
}));

<AsyncGreeter />;
