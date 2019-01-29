---
layout: guide
---

React exports a handful of utility types that may be useful to you when typing
advanced React patterns. In previous sections we have seen a few of them. The
following is a complete reference for each of these types along with some
examples for how/where to use them.

Table of contents:

- [`React.Node`](#toc-react-node)
- [`React.Element<typeof Component>`](#toc-react-element)
- [`React.ChildrenArray<T>`](#toc-react-childrenarray)
- [`React.AbstractComponent<Config, Instance>`](#toc-react-abstractcomponent)
- [`React.ComponentType<Props>`](#toc-react-componenttype)
- [`React.StatelessFunctionalComponent<Props>`](#toc-react-statelessfunctionalcomponent)
- [`React.ElementType`](#toc-react-elementtype)
- [`React.Key`](#toc-react-key)
- [`React.Ref<typeof Component>`](#toc-react-ref)
- [`React.ElementProps<typeof Component>`](#toc-react-elementprops)
- [`React.ElementConfig<typeof Component>`](#toc-react-elementconfig)
- [`React.ElementRef<typeof Component>`](#toc-react-elementref)
- [`React.Config<Props, DefaultProps>`](#toc-react-config)

These types are all exported as named type exports from the `react` module. If
you want to access them as members on the `React` object (e.g.
[`React.Node`](#toc-react-node) or
[`React.StatelessFunctionalComponent`](#toc-react-statelessfunctionalcomponent)) and
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

## `React.Node` <a class="toc" id="toc-react-node" href="#toc-react-node"></a>

This represents any node that can be rendered in a React application.
`React.Node` can be null, a boolean, a number, a string, a React
element, or an array of any of those types recursively.

If you need a return type for your component `render()` methods then you should use `React.Node`.
However, if you need a generic type for a children prop, use `?React.Node`;
children can be undefined, when `render()` can't return `undefined`.

Here is an example of `React.Node` being used as the return type to `render()`:

```js
class MyComponent extends React.Component<{}> {
  render(): React.Node {
    // ...
  }
}
```

It may also be used as the return type of a stateless functional component:

```js
function MyComponent(props: {}): React.Node {
  // ...
}
```

You don't need to annotate the return type of either your `render()` method or a
stateless functional component. However, if you want to annotate the return type
then `React.Node` is the generic to use.

Here is an example of `React.Node` as the prop type for children:

```js
function MyComponent({ children }: { children: React.Node }) {
  return <div>{children}</div>;
}
```

All `react-dom` JSX intrinsics have `React.Node` as their children type.
`<div>`, `<span>`, and all the rest.

The definition of `React.Node` can be roughly approximated with a
[`React.ChildrenArray<T>`](#toc-react-childrenarray):

```js
type Node = React.ChildrenArray<void | null | boolean | string | number | React.Element<any>>;
```

## `React.Element<typeof Component>` <a class="toc" id="toc-react-element" href="#toc-react-element"></a>

A React element is the type for the value of a JSX element:

```js
const element: React.Element<'div'> = <div />;
```

`React.Element<typeof Component>` is also the return type of
`React.createElement()`.

A `React.Element<typeof Component>` takes a single type argument,
`typeof Component`. `typeof Component` is the component type of the React
element. For an intrinsic element, `typeof Component` will be the string literal
for the intrinsic you used. Here are a few examples with DOM intrinsics:

```js
(<div />: React.Element<'div'>); // OK
(<span />: React.Element<'span'>); // OK
(<div />: React.Element<'span'>); // Error: div is not a span.
```

`typeof Component` can also be your React class component or stateless
functional component.

```js
class Foo extends React.Component<{}> {}
function Bar(props: {}) {}

(<Foo />: React.Element<typeof Foo>); // OK
(<Bar />: React.Element<typeof Bar>); // OK
(<Foo />: React.Element<typeof Bar>); // Error: Foo is not Bar
```

Take note of the `typeof`, it is required! `Foo` without `typeof` would be the
type of an instance of `Foo`. So: `(new Foo(): Foo)`. We want the type *of*
`Foo` not the type of an instance of `Foo`. So: `(Foo: typeof Foo)`.
`Class<Foo>` would also work here, but we prefer `typeof` for consistency with
stateless functional components.

We also need `typeof` for `Bar` because `Bar` is a value. So we want to get the
type *of* the value `Bar`. `(Bar: Bar)` is an error because `Bar` cannot be used
as a type, so the following is correct: `(Bar: typeof Bar)`.

## `React.ChildrenArray<T>` <a class="toc" id="toc-react-childrenarray" href="#toc-react-childrenarray"></a>

A React children array can be a single value or an array nested to any level.
It is designed to be used with the [`React.Children` API][].

[`React.Children` API]: https://reactjs.org/docs/react-api.html#reactchildren

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

## `React.AbstractComponent<Config, Instance>` <a class="toc" id="toc-react-abstractcomponent" href="#toc-react-abstractcomponent"></a>

`React.AbstractComponent<Config, Instance>` (v0.89.0+) represents a component with
a config of type Config and instance of type Instance.

Instance is optional and is mixed by default.


A class or function component with config `Config` may be used in places that expect
`React.AbstractComponent<Config>`.

This is Flow's most abstract representation of a React component, and is most useful for
writing HOCs and library definitions.

## `React.ComponentType<Props>` <a class="toc" id="toc-react-componenttype" href="#toc-react-componenttype"></a>

This is a union of a class component or a stateless functional component. This
is the type you want to use for functions that receive or return React
components such as higher-order components or other utilities.

Here is how you may use `React.ComponentType<Props>` with
[`React.Element<typeof Component>`](#toc-react-element) to construct a component
with a specific set of props:

```js
type Props = {
  foo: number,
  bar: number,
};

function createMyElement<C: React.ComponentType<Props>>(
  Component: C,
): React.Element<C> {
  return <Component foo={1} bar={2} />;
}
```

`React.ComponentType<Props>` does not include intrinsic JSX element types like
`div` or `span`. See [`React.ElementType`](#toc-react-elementtype) if you also want
to include JSX intrinsics.

The definition for `React.ComponentType<Props>` is roughly:

```js
type ComponentType<Props> =
  | React.StatelessFunctionalComponent<Props>
  | Class<React.Component<Props, any>>;
```

> **Note:** In 0.89.0+, React.ComponentType<Config> is an alias for React.AbstractComponent<Config, any>,
which represents a component with config type Config and any instance type.


## `React.StatelessFunctionalComponent<Props>` <a class="toc" id="toc-react-statelessfunctionalcomponent" href="#toc-react-statelessfunctionalcomponent"></a>

This is the type of a React stateless functional component.

The definition for `React.StatelessFunctionalComponent<Props>` is roughly:

```js
type StatelessFunctionalComponent<Props> =
  (props: Props) => React.Node;
```

There is a little bit more to the definition of
`React.StatelessFunctionalComponent<Props>` for context and props.

## `React.ElementType` <a class="toc" id="toc-react-elementtype" href="#toc-react-elementtype"></a>

Similar to [`React.ComponentType<Props>`](#toc-react-componenttype) except it also
includes JSX intrinsics (strings).

The definition for `React.ElementType` is roughly:

```js
type ElementType =
  | string
  | React.ComponentType<any>;
```

## `React.Key` <a class="toc" id="toc-react-key" href="#toc-react-key"></a>

The type of the key prop on React elements. It is a union of strings and
numbers defined as:

```js
type Key = string | number;
```

## `React.Ref<typeof Component>` <a class="toc" id="toc-react-ref" href="#toc-react-ref"></a>

The type of the [ref prop on React elements][]. `React.Ref<typeof Component>`
could be a string or a ref function.

[ref prop on React elements]: https://facebook.github.io/react/docs/refs-and-the-dom.html

The ref function will take one and only argument which will be the element
instance which is retrieved using
[`React.ElementRef<typeof Component>`](#toc-react-elementref) or null since
[React will pass null into a ref function when unmounting][].

[React will pass null into a ref function when unmounting]: https://facebook.github.io/react/docs/refs-and-the-dom.html#adding-a-ref-to-a-dom-element

Like [`React.Element<typeof Component>`](#toc-react-element), `typeof Component`
must be the type *of* a React component so you need to use `typeof` as in
`React.Ref<typeof MyComponent>`.

The definition for `React.Ref<typeof Component>` is roughly:

```js
type Ref<C> =
  | string
  | (instance: React.ElementRef<C> | null) => mixed;
```

## `React.ElementProps<typeof Component>` <a class="toc" id="toc-react-elementprops" href="#toc-react-elementprops"></a>

Gets the props for a React element type, *without* preserving the optionality of `defaultProps`.
`typeof Component` could be the type of a React class component, a stateless functional component, or a JSX intrinsic string.
This type is used for the `props` property on [`React.Element<typeof Component>`](#toc-react-element).

Like [`React.Element<typeof Component>`](#toc-react-element), `typeof Component` must be the
type *of* a React component so you need to use `typeof` as in
`React.ElementProps<typeof MyComponent>`.

> **Note:** Because [`React.ElementProps`](#toc-react-elementprops) does not preserve the optionality of `defaultProps`, [`React.ElementConfig`](#toc-react-elementconfig) (which does) is more often the right choice, especially for simple props pass-through as with [higher-order components](../hoc/#toc-supporting-defaultprops-with-react-elementconfig).

## `React.ElementConfig<typeof Component>` <a class="toc" id="toc-react-elementconfig" href="#toc-react-elementconfig"></a>

Like `React.ElementProps<typeof Component>` this utility gets the type of a
component's props but *preserves* the optionality of `defaultProps`!

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
({foo: 42}: React.ElementProps<typeof MyComponent>);

// `React.ElementConfig<>` does not require `foo` since it has a `defaultProp`.
({}: React.ElementConfig<typeof MyComponent>);
```

Like [`React.Element<typeof Component>`](#toc-react-element), `typeof Component` must be the
type *of* a React component so you need to use `typeof` as in
`React.ElementProps<typeof MyComponent>`.

## `React.ElementRef<typeof Component>` <a class="toc" id="toc-react-elementref" href="#toc-react-elementref"></a>

Gets the instance type for a React element. The instance will be different for
various component types:

- React class components will be the class instance. So if you had
  `class Foo extends React.Component<{}> {}` and used
  `React.ElementRef<typeof Foo>` then the type would be the instance of `Foo`.
- React stateless functional components do not have a backing instance and so
  `React.ElementRef<typeof Bar>` (when `Bar` is `function Bar() {}`) will give
  you the undefined type.
- JSX intrinsics like `div` will give you their DOM instance. For
  `React.ElementRef<'div'>` that would be `HTMLDivElement`. For
  `React.ElementRef<'input'>` that would be `HTMLInputElement`.

Like [`React.Element<typeof Component>`](#toc-react-element), `typeof Component` must be the
type *of* a React component so you need to use `typeof` as in
`React.ElementRef<typeof MyComponent>`.

## `React.Config<Props, DefaultProps>` <a class="toc" id="toc-react-config" href="#toc-react-config"></a>

Calculates a config object from props and default props. This is most useful for annotating
HOCs that are abstracted over configs. See our [docs on writing HOCs](../hoc) for more information.
