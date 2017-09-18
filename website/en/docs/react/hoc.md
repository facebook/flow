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
): (React.ComponentType<PropsInput>) => React.ComponentType<PropsOutput> {
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
): (React.ComponentType<PropsInput>) => React.ComponentType<PropsOutput> {
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
> Flow will not require you to add type annotations annotations, but it is a
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
and return a React component with the exact same type for props. To add a
prop we will use [an intersection](../../types/intersections/):

```js
import * as React from 'react';

function injectProp<Props: {}>(
  Component: React.ComponentType<{ foo: number } & Props>,
): React.ComponentType<Props> {
  // implementation...
}
```

Let's look at the type for our input component. In other words the type for
`MyInputComponent` in `injectProp(MyInputComponent)`.

```js
React.ComponentType<{ foo: number } & Props>
```

The type of props for this component is:

```js
{ foo: number } & Props
```

This uses [an intersection](../../types/intersection/) to say that the type for
props is everything in `Props` (which is the props type for our output
component) *except* for `foo` which has a type of `number`.

> **Note:** The intersection order is important here!

With this we can now use `injectProp()` to inject `foo`.

```js
import * as React from 'react';

function injectProp<Props: {}>(
  Component: React.ComponentType<{ foo: number } & Props>,
): React.ComponentType<Props> {
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
