---
layout: guide
---

A popular pattern in React is the [higher-order component pattern][], so it's
important that we can provide effective types for higher-order components in
Flow. If you don't already know what a higher-order component is then make sure
to read the [React documentation on higher-order components][] before
continuing.

[higher-order component pattern]: https://facebook.github.io/react/docs/higher-order-components.html
[React documentation on higher-order components]: https://facebook.github.io/react/docs/higher-order-components.html

Table of contents:

- [Flow version >= 0.89.0](#toc-hocs-as-of-0-89-0)
- [Flow version < 0.89.0](#toc-hocs-before-0-89-0)

## HOCs as of 0.89.0 <a class="toc" id="toc-hocs-as-of-0-89-0" href="#toc-hocs-as-of-0-89-0"></a>

In 0.89.0, we introduced [`React.AbstractComponent`](../types/#toc-react-abstractcomponent), which
makes writing HOCs and library definitions very expressive.

Let's take a look at how you can type some example HOCs.

### The Trivial HOC <a class="toc" id="toc-the-trivial-hoc" href="#toc-the-trivial-hoc"></a>

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

### Injecting Props <a class="toc" id="toc-injecting-props" href="#toc-injecting-props"></a>

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

### Preserving the Instance Type of a Component <a class="toc" id="toc-preserving-the-instance-type-of-a-component" href="#toc-preserving-the-instance-type-of-a-component"></a>

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

### Exporting Wrapped Components <a class="toc" id="toc-exporting-wrapped-components" href="#toc-exporting-wrapped-components"></a>

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

## HOCs before 0.89.0 <a class="toc" id="toc-hocs-before-0-89-0" href="#toc-hocs-before-0-89-0"></a>

To learn how to type higher-order components we will look to [Recompose][] for
examples of higher-order components. [Recompose][] is a popular React library
that provides many higher-order components. Let's see how you would type the
[`mapProps()` higher-order component from Recompose][].

[Recompose]: https://github.com/acdlite/recompose
[`mapProps()` higher-order component from Recompose]: https://github.com/acdlite/recompose/blob/0ff7cf36f35e97dbd422a6924c7e7eddd47d0d34/docs/API.md#mapprops

`mapProps()` takes a function that will transform the input props into some
output props. You can use `mapProps()` like this:

```js
function MyComponent({ bar }: { bar: number }) {
  return <div>{bar}</div>;
}

const MyEnhancedComponent = mapProps(
  ({ foo }) => ({ bar: foo + 1 }),
)(MyComponent);

<MyEnhancedComponent foo={1} />; // This will render the number 2.
```

For the type of `MyComponent` and the type of `MyEnhancedComponent` we will use
[`React.ComponentType<Props>`](../types/#toc-react-componenttype).
[`React.ComponentType<Props>`](../types/#toc-react-componenttype) is a union of
stateless functional components and class components where `Props` is the
defined type for the component's props.

We want `mapProps()` to return a function that will take a React component as
its first and only argument and return a React component.

```js
import * as React from 'react';

function mapProps(): (React.ComponentType<any>) => React.ComponentType<any> {
  return Component => {
    // implementation...
  };
}
```

> **Remember:** We are returning a *function type* here and we are taking no
> arguments. We will add the argument (which is also a function) in a later
> step.

To start we used `any` for our
[`React.ComponentType<Props>`](../types/#toc-react-componenttype)s' `Props` types! So
next we will use a [generic function type](../../types/generics/) to provide
better types than `any`.

```js
import * as React from 'react';

function mapProps<PropsInput: {}, PropsOutput: {}>(
  // TODO
): (React.ComponentType<PropsOutput>) => React.ComponentType<PropsInput> {
  return Component => {
    // implementation...
  };
}
```

Note that `PropsInput` and `PropsOutput` have bound of `{}`. (As expressed in
`PropsInput: {}` and `PropsOutput: {}`.) This means that `PropsInput` and
`PropsOutput` must be object types so we may treat them as such in the
implementation of `mapProps()`. If you do not add the `: {}` then you would not
be able to spread `PropsInput` or `PropsOutput`!

We have one last thing to do. Add a type for the mapper function which will take
`PropsInput` and return `PropsOutput` for `mapProps()`.

```js
import * as React from 'react';

function mapProps<PropsInput: {}, PropsOutput: {}>(
  mapperFn: (PropsInput) => PropsOutput,
): (React.ComponentType<PropsOutput>) => React.ComponentType<PropsInput> {
  return Component => {
    // implementation...
  };
}
```

Now you can use `mapProps()` with confidence that Flow is ensuring your types
are correct.

> **Note:** While when you use `mapProps()` in the following:
>
> ```
> const MyEnhancedComponent = mapProps(
>   ({ foo }) => ({ bar: foo + 1 }),
> )(MyComponent);
> ```
>
> Flow will not require you to add type annotations, but it is a
> smart idea to add annotations anyway. By adding type annotations you will get
> better error messages when something is broken. An annotated version of a
> `mapProps()` usage would look like:
>
> ```
> const MyEnhancedComponent = mapProps(
>   (props: PropsA): PropsB => /* ... */,
> )(MyComponent);
> ```
>
> Where `PropsA` and `PropsB` are your type annotations.

## Injecting Props With a Higher-order Component <a class="toc" id="toc-injecting-props-with-a-higher-order-component" href="#toc-injecting-props-with-a-higher-order-component"></a>

A common use case for higher-order components is to inject a prop. Like
a navigation prop, or in the case of [`react-redux` a `store` prop][]. How would
one type this? Let us start with a higher-order component that does not add any
new props:

[`react-redux` a `store` prop]: https://github.com/reactjs/react-redux/blob/master/docs/api.md#connectmapstatetoprops-mapdispatchtoprops-mergeprops-options

```js
import * as React from 'react';

function injectProp<Props: {}>(
  Component: React.ComponentType<Props>,
): React.ComponentType<Props> {
  // implementation...
}
```

This [generic function type](../../types/generics/) will take a React component
and return a React component with the exact same type for props. To remove a
prop from the returned component we will use
[`$Diff`](../../types/utilities/#toc-diff).

```js
import * as React from 'react';

function injectProp<Props: {}>(
  Component: React.ComponentType<Props>,
): React.ComponentType<$Diff<Props, { foo: number | void }>> {
  // implementation...
}
```

Let's look at the type for our output component. In other words the type for
`MyOutputComponent` in `const MyOutputComponent = injectProp(MyInputComponent)`.

```js
React.ComponentType<$Diff<Props, { foo: number | void }>
```

The type of props for this component is:

```js
$Diff<Props, { foo: number | void }>
```

This uses [`$Diff`](../../types/utilities/#toc-diff) to say that the type for
props is everything in `Props` (which is the props type for our output
component) *except* for `foo` which has a type of `number`.

> **Note:** If `foo` does not exist in `Props` you will get an error!
> `$Diff<{}, { foo: number }>` will be an error. To work around this use a union
> with `void`, see: `$Diff<{}, { foo: number | void }>`. An optional prop will
> not completely remove `foo`. `$Diff<{ foo: number }, { foo?: number }>`
> is `{ foo?: number }` instead of `{}`.

With this we can now use `injectProp()` to inject `foo`.

```js
import * as React from 'react';

function injectProp<Props: {}>(
  Component: React.ComponentType<Props>,
): React.ComponentType<$Diff<Props, { foo: number | void }>> {
  return function WrapperComponent(props: Props) {
    return <Component {...props} foo={42} />;
  };
}

class MyComponent extends React.Component<{
  a: number,
  b: number,
  foo: number,
}> {}

const MyEnhancedComponent = injectProp(MyComponent);

// We don't need to pass in `foo` even though `MyComponent` requires it.
<MyEnhancedComponent a={1} b={2} />;
```

> **Note:** Remember that the generic type, `Props`, needs the bound `{}`. As in
> `Props: {}`. Otherwise you would not be able to spread `Props` in
> `<Component {...props} foo={42} />`.

## Supporting `defaultProps` With `React.ElementConfig<>` <a class="toc" id="toc-supporting-defaultprops-with-react-elementconfig" href="#toc-supporting-defaultprops-with-react-elementconfig"></a>

The higher-order-components we've typed so far will all make `defaultProps`
required. To preserve the optionality of `defaultProps` you can use
[`React.ElementConfig<typeof Component>`](../types/#toc-react-elementconfig).
Your enhancer function will need a generic type for your component. Like this:

```js
function myHOC<Props, Component: React.ComponentType<Props>>(
  WrappedComponent: Component
): React.ComponentType<React.ElementConfig<Component>> {
  return props => <WrappedComponent {...props} />;
}
```

Notice here how we used `React.ComponentType<React.ElementConfig<Component>>`
as the output component type instead of `React.ComponentType<Props>` as we've
seen in previous examples.
