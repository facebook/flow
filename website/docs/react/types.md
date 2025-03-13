---
title: Type Reference
slug: /react/types
---

React exports a handful of utility types that may be useful to you when typing
advanced React patterns. In previous sections we have seen a few of them. The
following is a complete reference for each of these types along with some
examples for how/where to use them.

These types are all exported as named type exports from the `react` module. If
you want to access them as members on the `React` object (e.g.
[`React.Node`](#toc-react-node)) and
you are importing React as an ES module then you should import `React` as a
namespace:

```js
import * as React from 'react';
```

If you are using CommonJS you can also require React:

```js
const React = require('react');
```

You can also use named type imports in either an ES module environment or a
CommonJS environment:

```js
import type {Node} from 'react';
```

We will refer to all the types in the following reference as if we imported them
with:

```js
import * as React from 'react';
```

> **Note:** While importing React with a default import works:
>
> ```
> import React from 'react';
> ```
>
> You will have access to all of the values that React exports, but you will
> **not** have access to the types documented below! This is because Flow will
> not add types to a default export since the default export could be any value
> (like a number). Flow will add exported named types to an ES namespace object
> which you can get with `import * as React from 'react'` since Flow knows if
> you export a value with the same name as an exported type.
>
> Again, if you import React with: `import React from 'react'` you will be able
> to access `React.Component`, `React.createElement()`, `React.Children`, and
> other JavaScript *values*. However, you will not be able to access
> [`React.Node`](#toc-react-node), [`React.ChildrenArray`](#toc-react-childrenarray) or
> other Flow *types*. You will need to use a named type import like:
> `import type {Node} from 'react'` in addition to your default import.

## `React.Node` {#toc-react-node}

This represents any node that can be rendered in a React application.
`React.Node` can be null, a boolean, a number, a string, a React
element, or an array of any of those types recursively.

`React.Node` is a good default to use to annotate the return type of a function component
and class render methods. You can also use it to type elements your component takes in as children.

Here is an example of `React.Node` being used as the return type to a function component:

```js
function MyComponent(props: {}): React.Node {
  // ...
}
```

It may also be used as the return type of a class `render` method:
```js
class MyComponent extends React.Component<{}> {
  render(): React.Node {
    // ...
  }
}
```

Here is an example of `React.Node` as the prop type for children:

```js
function MyComponent({ children }: { children: React.Node }) {
  return <div>{children}</div>;
}
```

All `react-dom` JSX intrinsics have `React.Node` as their children type.
`<div>`, `<span>`, and all the rest.

## `React.MixedElement` {#toc-react-mixedelement}

The most general type of all React elements (similar to `mixed` for all values).

A common use case of this type is when we want to annotate an element with a type that hides the element details. For example
```js
const element: React.MixedElement = <div />;
```

## `React.ChildrenArray<T>` {#toc-react-childrenarray}

A React children array can be a single value or an array nested to any level.
It is designed to be used with the [`React.Children` API](https://react.dev/reference/react/Children).

For example if you want to get a normal JavaScript array from a
`React.ChildrenArray<T>` see the following example:

```js
import * as React from 'react';

// A children array can be a single value...
const children: React.ChildrenArray<number> = 42;
// ...or an arbitrarily nested array.
const children: React.ChildrenArray<number> = [[1, 2], 3, [4, 5]];

// Using the `React.Children` API can flatten the array.
const array: Array<number> = React.Children.toArray(children);
```

## `React.AbstractComponent<Config, Instance, Renders>` {#toc-react-abstractcomponent}

:::tip
In Flow v0.243.0+, consider using [Component Types](../component-types) instead, which will make
it easier to migrate your Flow code to React 19. The type will be removed in Flow v0.251.0.
:::

`React.AbstractComponent<Config, Instance, Renders>` represents a component with
a config of type Config and instance of type Instance that renders something of type Renders.

The `Config` of a component is the type of the object you need to pass in to JSX in order
to create an element with that component. The `Instance` of a component is the type of the value
that is written to the `current` field of a ref object passed into the `ref` prop in JSX.
`Renders` is a [Component Syntax](../component-syntax) feature that allows you to specify what your
component renders via [Render Types](../render-types)

Config is required, but Instance is optional and defaults to mixed and Renders is optional and defaults to React.Node.

A class or function component with config `Config` may be used in places that expect
`React.AbstractComponent<Config>`.

This is Flow's most abstract representation of a React component, and is most useful for
writing HOCs and library definitions.

## `React.ComponentType<Config>` {#toc-react-componenttype}

This is the same as [`React.AbstractComponent`](#toc-react-abstractcomponent), but only specifies the first type argument.

## `React.ElementType` {#toc-react-elementtype}

Similar to [`React.AbstractComponent<Props>`](#toc-react-abstractcomponent) except it also
includes JSX intrinsics (strings).

The definition for `React.ElementType` is roughly:

```js
type ElementType =
  | string
  | React.ComponentType<empty>;
```

## `React.Key` {#toc-react-key}

The type of the key prop on React elements. It is a union of strings and
numbers defined as:

```js
type Key = string | number;
```

## `React.Ref<typeof Component>` {#toc-react-ref}

The type of the [ref prop on React elements][]. `React.Ref<typeof Component>`
could be a string, ref object, or ref function.

[ref prop on React elements]: https://react.dev/learn/manipulating-the-dom-with-refs

The ref function will take one and only argument which will be the element
instance which is retrieved using
[`React.ElementRef<typeof Component>`](#toc-react-elementref) or null since
[React will pass null into a ref function when unmounting][].

[React will pass null into a ref function when unmounting]: https://react.dev/learn/manipulating-the-dom-with-refs#how-to-manage-a-list-of-refs-using-a-ref-callback

Like [`React.Element<typeof Component>`](#toc-react-element), `typeof Component`
must be the type *of* a React component so you need to use `typeof` as in
`React.Ref<typeof MyComponent>`.

The definition for `React.Ref<typeof Component>` is roughly:

```js
type Ref<C> =
  | string
  | (instance: React.ElementRef<C> | null) => mixed;
  | { -current: React$ElementRef<ElementType> | null, ... }
```
## `React.PropsOf<Component>` {#toc-react-propsof}
When `Component` is written using [Component Syntax](../component-syntax), `React.PropsOf<Component>`
gives you the type of an object that you must pass in to instantiate `Component` with JSX.
Importantly, the props with defaults are optional in the resulting type.

For example:
```js flow-check
import * as React from 'react';

component MyComponent(foo: number, bar: string = 'str') {
  return null;
}

// Only foo is required
({foo: 3}) as React.PropsOf<MyComponent>;
```

## `React.ElementConfig<typeof Component>` {#toc-react-elementconfig}

Like [React.PropsOf](#toc-react-propsof), this utility gets the type of the object that you must pass in to a
component in order to instantiate it via `createElement()` or `jsx()`. While `PropsOf` takes in an element of
a component, which is convenient when using [Component Syntax](../component-syntax), `ElementConfig` takes in the type of a component
instead. `typeof Component` must be the type *of* a React component so you need to use `typeof` as in
`React.ElementConfig<typoef Component>`.

Importantly, props with defaults are optional in the resulting type.

For example,

```js
import * as React from 'react';

class MyComponent extends React.Component<{foo: number}> {
  static defaultProps = {foo: 42};

  render() {
    return this.props.foo;
  }
}

// `React.ElementProps<>` requires `foo` even though it has a `defaultProp`.
({foo: 42}) as React.ElementProps<typeof MyComponent>;

// `React.ElementConfig<>` does not require `foo` since it has a `defaultProp`.
({}) as React.ElementConfig<typeof MyComponent>;
```

Like [`React.Element<typeof Component>`](#toc-react-element), `typeof Component` must be the
type *of* a React component so you need to use `typeof` as in
`React.ElementProps<typeof MyComponent>`.


## `React.ElementProps<typeof Component>` {#toc-react-elementprops}

> **Note:** Because [`React.ElementProps`](#toc-react-elementprops) does not preserve the optionality of `defaultProps`, [`React.ElementConfig`](#toc-react-elementconfig) (which does) is more often the right choice, especially for simple props pass-through as with [higher-order components](../hoc/#toc-supporting-defaultprops-with-react-elementconfig).
You probably should not use ElementProps.

Gets the props for a React element type, *without* preserving the optionality of `defaultProps`.
`typeof Component` could be the type of a React class component, a function component, or a JSX intrinsic string.
This type is used for the `props` property on [`React.Element<typeof Component>`](#toc-react-element).

Like [`React.Element<typeof Component>`](#toc-react-element), `typeof Component` must be the
type *of* a React component so you need to use `typeof` as in
`React.ElementProps<typeof MyComponent>`.
## `React.RefOf<Component>` {#toc-react-refof}

When using [Component Syntax](../component-syntax), `React.RefOf<Component>` will give you
the type of the `current` field on the `ref` prop of the component. If there is no `ref` prop
on the component it will return `void`.

## `React.ElementRef<typeof Component>` {#toc-react-elementref}

Gets the instance type for a React element. The instance will be different for
various component types:

- `component(ref: React.RefSetter<Instance>)` will return the `Instance` type.
- React class components will be the class instance. So if you had
  `class Foo extends React.Component<{}> {}` and used
  `React.ElementRef<typeof Foo>` then the type would be the instance of `Foo`.
- React function components do not have a backing instance and so
  `React.ElementRef<typeof Bar>` (when `Bar` is `function Bar() {}`) will give
  you the void type.
- JSX intrinsics like `div` will give you their DOM instance. For
  `React.ElementRef<'div'>` that would be `HTMLDivElement`. For
  `React.ElementRef<'input'>` that would be `HTMLInputElement`.

Like [`React.Element<typeof Component>`](#toc-react-element), `typeof Component` must be the
type *of* a React component so you need to use `typeof` as in
`React.ElementRef<typeof MyComponent>`.

## `React.Config<Props, DefaultProps>` {#toc-react-config}

:::warning
This type will be removed in 0.257.0. This type is usually only useful for legacy class components.
You can create your own equivalent type with

```flow
type ReactConfigShim<Props, DefaultProps> = $ReadOnly<{
  ...Omit<Props, $Keys<DefaultProps>>, ...Partial<DefaultProps>
}>;
:::

Calculates a config object from props and default props.

## `ExactReactElement_DEPRECATED<typeof Component>` {#toc-react-element}

:::warning
This is an exact replacement of the removed 'React.Element' type since 0.245.
You should use `React.MixedElement` or `React.Node` instead.
If you want to enforce design system constraints, use [render types](../render-types) instead.
:::

A React element is the type for the value of a JSX element:

```js
const element: ExactReactElement_DEPRECATED<'div'> = <div />;
```

`ExactReactElement_DEPRECATED<typeof Component>` is also the return type of
`React.createElement()`/`React.jsx()`.

A `ExactReactElement_DEPRECATED` takes a single type argument,
`typeof Component`. `typeof Component` is the component type of the React
element. For an intrinsic element, `typeof Component` will be the string literal
for the intrinsic you used. Here are a few examples with DOM intrinsics:

```js
<div /> as ExactReactElement_DEPRECATED<'div'>; // OK
<span /> as ExactReactElement_DEPRECATED<'span'>; // OK
<div /> as ExactReactElement_DEPRECATED<'span'>; // Error: div is not a span.
```

`typeof Component` can also be your React class component or function component.

```js
function Foo(props: {}) {}
class Bar extends React.Component<{}> {}

<Foo /> as ExactReactElement_DEPRECATED<typeof Foo>; // OK
<Bar /> as ExactReactElement_DEPRECATED<typeof Bar>; // OK
<Foo /> as ExactReactElement_DEPRECATED<typeof Bar>; // Error: Foo is not Bar
```

Take note of the `typeof`, it is required! We want to get the
type *of* the value `Foo`. `Foo as Foo` is an error because `Foo` cannot be used
as a type, so the following is correct: `Foo as typeof Foo`.

`Bar` without `typeof` would be the type of an instance of `Bar`: `new Bar() as Bar`.
We want the type *of* `Bar` not the type of an instance of `Bar`.
`Class<Bar>` would also work here, but we prefer `typeof` for consistency
with function components.
