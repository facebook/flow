---
title: Higher-order Components
slug: /react/hoc
---

:::danger
Higher-order components are discouraged in modern React code and will not be updated for [Component Syntax](../component-syntax).
Consider using a hook to accomplish your task instead.
:::

A popular pattern in React is the [higher-order component pattern][], so it's
important that we can provide effective types for higher-order components in
Flow. If you don't already know what a higher-order component is then make sure
to read the [React documentation on higher-order components][] before
continuing.

[higher-order component pattern]: https://facebook.github.io/react/docs/higher-order-components.html
[React documentation on higher-order components]: https://facebook.github.io/react/docs/higher-order-components.html

You can make use of the [Component Types](../component-types/) to annotate your higher order components.

### The Trivial HOC {#toc-the-trivial-hoc}

Let's start with the simplest HOC:

```js flow-check
import * as React from 'react';

function trivialHOC<Config: {...}>(
  Component: component(...Config),
): component(...Config) {
  return Component;
}
```

This is a basic template for what your HOCs might look like. At runtime, this HOC doesn't
do anything at all. Let's take a look at some more complex examples.

### Injecting Props {#toc-injecting-props}

A common use case for higher-order components is to inject a prop.
The HOC automatically sets a prop and returns a component which no longer requires
that prop. For example, consider a navigation prop. How would one type this?

To remove a prop from the config, we can take a component that includes the
prop and return a component that does not. It's best to construct these
types using object type spread.

```js flow-check
import * as React from 'react';

type InjectedProps = {foo: number}

function injectProp<Config>(
  Component: component(...{...$Exact<Config>, ...InjectedProps})
): component(...$Exact<Config>) {
  return function WrapperComponent(
    props: Config,
  ) {
    return <Component {...props} foo={42} />;
  };
}

function MyComponent(props: {
  a: number,
  b: number,
  ...InjectedProps,
}): React.Node {}

const MyEnhancedComponent = injectProp(MyComponent);

// We don't need to pass in `foo` even though `MyComponent` requires it:
<MyEnhancedComponent a={1} b={2} />; // OK

// We still require `a` and `b`:
<MyEnhancedComponent a={1} />; // ERROR
```

### Preserving the Instance Type of a Component {#toc-preserving-the-instance-type-of-a-component}

Recall that the instance type of a function component is `void`. Our example
above wraps a component in a function, so the returned component has the instance
type `void`.

```js flow-check
import * as React from 'react';

type InjectedProps = {foo: number}

function injectProp<Config>(
  Component: component(...{...$Exact<Config>, ...InjectedProps})
): component(...$Exact<Config>) {
  return function WrapperComponent(
    props: Config,
  ) {
    return <Component {...props} foo={42} />;
  };
}

// A class component in this example
class MyComponent extends React.Component<{
  a: number,
  b: number,
  ...InjectedProps,
}> {}

const MyEnhancedComponent = injectProp(MyComponent);

// If we create a ref object for the component, it will never be assigned
// an instance of MyComponent!
const ref = React.createRef<MyComponent>();

// Error, mixed is incompatible with MyComponent.
<MyEnhancedComponent ref={ref} a={1} b={2} />;
```

We get this error message because component type doesn't declare the `ref` prop,
so it is treated as `React.RefSetter<void>`. If we wanted to preserve the instance type
of the component, we can use [`React.forwardRef`](https://reactjs.org/docs/forwarding-refs.html):

```js flow-check
import * as React from 'react';

type InjectedProps = {foo: number}

function injectAndPreserveInstance<Config: {...}, Instance>(
  Component: component(ref?: React.RefSetter<Instance>, ...{...$Exact<Config>, ...InjectedProps})
): component(ref?: React.RefSetter<Instance>, ...$Exact<Config>) {
  return React.forwardRef<$Exact<Config>, Instance>((props, ref) =>
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

```js flow-check
import * as React from 'react';

function trivialHOC<Config: {...}>(
  Component: component(...Config),
): component(...Config) {
  return Component;
}

type Props = $ReadOnly<{bar: number, foo?: number}>;

function MyComponent({bar, foo = 3}: Props): React.Node {}

export const MyEnhancedComponent = trivialHOC(MyComponent); // ERROR
```

You can add an annotation to your exported component using component types:

```js flow-check
import * as React from 'react';

function trivialHOC<Config: {...}>(
  Component: component(...Config),
): component(...Config) {
  return Component;
}

type Props = $ReadOnly<{bar: number, foo?: number}>;

function MyComponent({bar, foo = 3}: Props): React.Node {}

export const MyEnhancedComponent: component(...Props) = trivialHOC(MyComponent); // OK
```
