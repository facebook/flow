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

## Functional Components {#toc-functional-components}

Adding Flow types to a functional component is the same as [adding types to a
standard function]. Just create an object type for the props and Flow will
ensure that the props passed to the component match up with what is expected.

[adding types to a standard function]: ../../types/functions/

```js flow-check
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

> **Note:** We import `React` as a namespace here with
> `import * as React from 'react'` instead of as a default with
> `import React from 'react'`. When importing React as an ES module you may use
> either style, but importing as a namespace gives you access to React's
> [utility types](../types).


### Adding Default Props to Functional Components {#toc-adding-default-props-to-functional-components}

A nice pattern to add default props to functional components is to use
[destructuring with default values][]. By destructuring the props in the
function parameter, you can assign a value to any props that are not passed
to the component (or passed with the value `undefined`).

[destructuring with default values]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment#default_value

```js flow-check
import * as React from 'react';

type Props = {
  foo?: number, // foo is optional to pass in.
  bar: string, // bar is required.
};

function MyComponent({foo = 42, bar}: Props) {
  // Flow knows that foo is not null or undefined
  const baz = foo + 1;
}

// And we don't need to include foo.
<MyComponent bar={"abc"} />;
```

## Class Components {#toc-class-components}

To Flowify a class component, the type of the props can be passed as the first
argument to the `React.Component` type. This will have the same effect as adding types
to the `props` parameter of a function component.

```js flow-check
import * as React from 'react';

type Props = {
  foo: number,
  bar?: string,
};

class MyComponent extends React.Component<Props> {
  render(): React.Node {
    this.props.doesNotExist; // Error! You did not define a `doesNotExist` prop.

    return <div>{this.props.bar}</div>;
  }
}

<MyComponent foo={42} />;
```

Now wherever we use `this.props` in our React component Flow will treat it as
the `Props` type we defined.

> **Note:** If you don't need to use the `Props` type again you could also
> define it inline: `extends React.Component<{ foo: number, bar?: string }>`.

`React.Component<Props, State>` is a [generic type][] that takes two type
arguments. Props and state. The second type argument, `State`, is optional. By
default it is undefined so you can see in the example above we did not include
`State`. We will learn more about state in the next section...

[generic type]: ../../types/generics/

### Adding State {#toc-adding-state}

To add a type for state to your React class component: create a new object
type, in the example below we name it `State`, and pass it as the second type
argument to `React.Component`.

```js flow-check
import * as React from 'react';

type Props = { /* ... */ };

type State = {
  count: number,
};

class MyComponent extends React.Component<Props, State> {
  state: State = {
    count: 0,
  };

  componentDidMount() {
    setInterval(() => {
      this.setState(prevState => ({
        count: prevState.count + 1,
      }));
    }, 1000);
  }

  render(): React.Node {
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

### Using Default Props for Class Components {#toc-using-default-props-for-class-components}

React supports the notion of `defaultProps` which you can think of as default
function arguments. When you create an element and do not include a prop
which has a default then React will substitute that prop with its corresponding
value from `defaultProps`. Flow supports this notion as well. To type default
props add a `static defaultProps` property to your class.

```js flow-check
import * as React from 'react';

type Props = {
  foo: number, // foo is required.
  bar: string, // bar is required.
};

class MyComponent extends React.Component<Props> {
  static defaultProps: { foo: number } = {
    foo: 42, // ...but we have a default prop for foo.
  };
}

// So we don't need to include foo.
<MyComponent bar={"abc"} />
```

> **Note:** You don't need to make `foo` nullable in your `Props` type. Flow
> will make sure that `foo` is optional if you have a default prop for `foo`.

Flow will infer the type of your default props from `static defaultProps` so you
don't have to add any type annotations to use default props.

If you would like to add a type annotation to `defaultProps` you can define the
type as
```js flow-check
type DefaultProps = {
  foo: number,
};
```
and spread that into the `Props` type:
```js
type Props = {
  ...DefaultProps,
  bar: string,
};
```
This way you avoid duplicating the properties that happen to have a default value.

> **Note:** You can also apply this format of default props to functional components
> by adding a `defaultProps` property to a the component function. However, it is generally
> simpler to use the destructuring pattern described above.
> ```js flow-check
> function MyComponent(props: {foo: number}) {}
> MyComponent.defaultProps = {foo: 42};
> ```
