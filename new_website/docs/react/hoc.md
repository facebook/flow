---
title: Higher-order Components
slug: /react/hoc
---

A popular pattern in React is the [higher-order component pattern][], so it's
important that we can provide effective types for higher-order components in
Flow. If you don't already know what a higher-order component is then make sure
to read the [React documentation on higher-order components][] before
continuing.

[higher-order component pattern]: https://facebook.github.io/react/docs/higher-order-components.html
[React documentation on higher-order components]: https://facebook.github.io/react/docs/higher-order-components.html

In 0.89.0, we introduced [`React.AbstractComponent`](../types/#toc-react-abstractcomponent), which
gives you more expressive power when writing HOCs and library definitions.

Let's take a look at how you can type some example HOCs.

### The Trivial HOC {#toc-the-trivial-hoc}

Let's start with the simplest HOC:

```js
//@flow
import * as React from 'react';

function trivialHOC<Config: {}>(
  Component: React.AbstractComponent<Config>
): React.AbstractComponent<Config> {
  return Component;
}
```

This is a basic template for what your HOCs might look like. At runtime, this HOC doesn't
do anything at all. Let's take a look at some more complex examples.

### Injecting Props {#toc-injecting-props}

A common use case for higher-order components is to inject a prop.
The HOC automatically sets a prop and returns a component which no longer requires
that prop. For example, consider a navigation prop, or in the case of
[`react-redux` a `store` prop][]. How would one type this?

[`react-redux` a `store` prop]: https://github.com/reactjs/react-redux/blob/master/docs/api.md#connectmapstatetoprops-mapdispatchtoprops-mergeprops-options

To remove a prop from the config, we can take a component that includes the
prop and return a component that does not. It's best to construct these
types using object type spread.

```js
//@flow
import * as React from 'react';

type InjectedProps = {| foo: number |}

function injectProp<Config>(
  Component: React.AbstractComponent<{| ...Config, ...InjectedProps |}>
): React.AbstractComponent<Config> {
  return function WrapperComponent(
    props: Config,
  ) {
    return <Component {...props} foo={42} />;
  };
}

class MyComponent extends React.Component<{|
  a: number,
  b: number,
  ...InjectedProps,
|}> {}

const MyEnhancedComponent = injectProp(MyComponent);

// We don't need to pass in `foo` even though `MyComponent` requires it.
<MyEnhancedComponent a={1} b={2} />;
```

### Preserving the Instance Type of a Component {#toc-preserving-the-instance-type-of-a-component}

Recall that the instance type of a function component is `void`. Our example
above wraps a component in a function, so the returned component has the instance
type `void`.

```js
//@flow
import * as React from 'react';

type InjectedProps = {| foo: number |}

function injectProp<Config>(
  Component: React.AbstractComponent<{| ...Config, ...InjectedProps |}>
): React.AbstractComponent<Config> {
  return function WrapperComponent(
    props: Config,
  ) {
    return <Component {...props} foo={42} />;
  };
}

class MyComponent extends React.Component<{|
  a: number,
  b: number,
  ...InjectedProps,
|}> {}

const MyEnhancedComponent = injectProp(MyComponent);

// If we create a ref object for the component, it will never be assigned
// an instance of MyComponent!
const ref = React.createRef<MyComponent>();

// Error, mixed is incompatible with MyComponent.
<MyEnhancedComponent ref={ref} a={1} b={2} />;
```

We get this error message because `React.AbstractComponent<Config>` doesn't set the `Instance` type
parameter, so it is automatically set to `mixed`. If we wanted to preserve the instance type
of the component, we can use [`React.forwardRef`](https://reactjs.org/docs/forwarding-refs.html):

```js
//@flow
import * as React from 'react';

type InjectedProps = {| foo: number |}

function injectAndPreserveInstance<Config, Instance>(
  Component: React.AbstractComponent<{| ...Config, ...InjectedProps |}, Instance>
): React.AbstractComponent<Config, Instance> {
  return React.forwardRef<Config, Instance>((props, ref) =>
      <Component ref={ref} foo={3} {...props} />
  );
}

class MyComponent extends React.Component<{
  a: number,
  b: number,
  ...InjectedProps,
}> {}

const MyEnhancedComponent = injectAndPreserveInstance(MyComponent);

const ref = React.createRef<MyComponent>();

// All good! The ref is forwarded.
<MyEnhancedComponent ref={ref} a={1} b={2} />;
```

### Exporting Wrapped Components {#toc-exporting-wrapped-components}

If you try to export a wrapped component, chances are that you'll run into a missing annotation error:
```js
//@flow
import * as React from 'react';

function trivialHOC<Config: {}>(
  Component: React.AbstractComponent<Config>,
): React.AbstractComponent<Config> {
  return Component;
}

type DefaultProps = {| foo: number |};
type Props = {...DefaultProps, bar: number};

class MyComponent extends React.Component<Props> {
  static defaultProps: DefaultProps = {foo: 3};
}

// Error, missing annotation for Config.
const MyEnhancedComponent = trivialHOC(MyComponent);

module.exports = MyEnhancedComponent;
```

If your component has no `defaultProps`, you can use `Props` as a type argument for `Config`.

If your component does have `defaultProps`, you don't want to just add `Props`
as a type argument to `trivialHOC` because that will get rid of the
`defaultProps` information that flow has about your component.

This is where [`React.Config<Props, DefaultProps>`](../types/#toc-react-config)
comes in handy! We can use the type for Props and DefaultProps to calculate the
`Config` type for our component.

```js
//@flow
import * as React from 'react';

function trivialHOC<Config: {}>(
  Component: React.AbstractComponent<Config>,
): React.AbstractComponent<Config> {
  return Component;
}

type DefaultProps = {| foo: number |};
type Props = {...DefaultProps, bar: number};

class MyComponent extends React.Component<Props> {
  static defaultProps: DefaultProps = {foo: 3};
}

const MyEnhancedComponent = trivialHOC<React.Config<Props, DefaultProps>>(MyComponent);

// Ok!
module.exports = MyEnhancedComponent;
```
