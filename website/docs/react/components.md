---
title: Components
slug: /react/components
---

Adding Flow types to your React components is incredibly powerful. After typing
your component, Flow will statically ensure that you are using the component in
the way it was designed to be used.

Early in React's history the library provided [`PropTypes`][] which performed
basic runtime checks. Flow is much more powerful as it can tell you when you are
misusing a component without running your code.

[`PropTypes`]: https://facebook.github.io/react/docs/typechecking-with-proptypes.html

There are some Babel plugins which will generate `PropTypes` from Flow types
such as [`babel-plugin-react-flow-props-to-prop-types`][] if you want both
static and runtime checks.

[`babel-plugin-react-flow-props-to-prop-types`]: https://github.com/thejameskyle/babel-plugin-react-flow-props-to-prop-types

## Class Components {#toc-class-components}

Before we show how to type a React class component with Flow, let us first show
how you would write a React class component *without* Flow but with React's prop
types. You would extend `React.Component` and add a static `propTypes` property.

```js
import React from 'react';
import PropTypes from 'prop-types';

class MyComponent extends React.Component {
  static propTypes = {
    foo: PropTypes.number.isRequired,
    bar: PropTypes.string,
  };

  render() {
    return <div>{this.props.bar}</div>;
  }
}
```

Now, let's Flowify the component we just wrote:

```js
import * as React from 'react';

type Props = {
  foo: number,
  bar?: string,
};

class MyComponent extends React.Component<Props> {
  render() {
    this.props.doesNotExist; // Error! You did not define a `doesNotExist` prop.

    return <div>{this.props.bar}</div>;
  }
}

<MyComponent foo={42} />;
```

We removed our dependency on `prop-types` and added a Flow object type named
`Props` with the same shape as the prop types but using Flow's static type
syntax. Then we passed our new `Props` type into `React.Component` as a type
argument.

Now if you try to use `<MyComponent>` with a string for `foo` instead of a
number you will get an error.

Now wherever we use `this.props` in our React component Flow will treat it as
the `Props` type we defined.

> **Note:** If you don't need to use the `Props` type again you could also
> define it inline: `extends React.Component<{ foo: number, bar?: string }>`.

> **Note:** We import `React` as a namespace here with
> `import * as React from 'react'` instead of as a default with
> `import React from 'react'`. When importing React as an ES module you may use
> either style, but importing as a namespace gives you access to React's
> [utility types](../types).

`React.Component<Props, State>` is a [generic type][] that takes two type
arguments. Props and state. The second type argument, `State`, is optional. By
default it is undefined so you can see in the example above we did not include
`State`. We will learn more about state in the next section...

[generic type]: ../../types/generics/

### Adding State {#toc-adding-state}

To add a type for state to your React class component then create a new object
type, in the example below we name it `State`, and pass it as the second type
argument to `React.Component`.

```js
import * as React from 'react';

type Props = { /* ... */ };

type State = {
  count: number,
};

class MyComponent extends React.Component<Props, State> {
  state = {
    count: 0,
  };

  componentDidMount() {
    setInterval(() => {
      this.setState(prevState => ({
        count: prevState.count + 1,
      }));
    }, 1000);
  }

  render() {
    return <div>Count: {this.state.count}</div>;
  }
}

<MyComponent />;
```

In the example above we are using a [React `setState()` updater function][] but
you could also pass a partial state object to `setState()`.

[React `setState()` updater function]: https://facebook.github.io/react/docs/state-and-lifecycle.html#state-updates-may-be-asynchronous

> **Note:** If you don't need to use the `State` type again you could also
> define it inline: `extends React.Component<{}, { count: number }>`.

### Using Default Props {#toc-using-default-props}

React supports the notion of `defaultProps` which you can think of as default
function arguments. When you create an element and you did not include a prop
with a default then React will substitute that prop with its corresponding
value from `defaultProps`. Flow supports this notion as well. To type default
props add a `static defaultProps` property to your class.

```js
import * as React from 'react';

type Props = {
  foo: number, // foo is required.
  bar: string, // bar is required.
};

class MyComponent extends React.Component<Props> {
  static defaultProps = {
    foo: 42, // ...but we have a default prop for foo.
  };
}

// So we don't need to include foo.
<MyComponent bar={"abc"} />
```

Flow will infer the type of your default props from `static defaultProps` so you
don't have to add any type annotations to use default props.

> **Note:** You don't need to make `foo` nullable in your `Props` type. Flow
> will make sure that `foo` is optional if you have a default prop for `foo`.

If you would like to add a type annotation to `defaultProps` you can define the
type as
```js
type DefaultProps = {|
  foo: number,
|}
```
and spread that into the `Props` type:
```js
type Props = {
  ...DefaultProps,
  bar: string,
}
```
This way you avoid duplicating the properties that happen to have a default value.

## Stateless Functional Components {#toc-stateless-functional-components}

In addition to classes, React also supports stateless functional components.
You type these components like you would type a function:

```js
import * as React from 'react';

type Props = {
  foo: number,
  bar?: string,
};

function MyComponent(props: Props) {
  props.doesNotExist; // Error! You did not define a `doesNotExist` prop.

  return <div>{props.bar}</div>;
}

<MyComponent foo={42} />
```

### Using Default Props for Functional Components {#toc-using-default-props-for-functional-components}

React also supports default props on stateless functional components. Similarly
to class components, default props for stateless functional components will
work without any extra type annotations.

```js
import * as React from 'react';

type Props = {
  foo: number, // foo is required.
};

function MyComponent(props: Props) {}

MyComponent.defaultProps = {
  foo: 42, // ...but we have a default prop for foo.
};

// So we don't need to include foo.
<MyComponent />;
```

> **Note:** You don't need to make `foo` nullable in your `Props` type. Flow
> will make sure that `foo` is optional if you have a default prop for `foo`.
