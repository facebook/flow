---
layout: guide
---

React elements can have zero, one, or many children. Being able to type these
children with Flow allows you to build expressive APIs with React children.

Generally, the type you should first try when adding a type for the children of
your React component is [`React.Node`](../types/#toc-react-node).

Like this:

```js
import * as React from 'react';

type Props = {
  children?: React.Node,
};

function MyComponent(props: Props) {
  return <div>{props.children}</div>;
}
```

> **Note:** You need to use `import * as React from 'react'` here instead of
> `import React from 'react'` to get access to the
> [`React.Node`](../types/#toc-react-node) type. We explain why that is in the
> [React Type Reference](../types/).

However, if you want to do anything more powerful with the React children API
then you will need a strong intuition of how React handles children. Let us look
at a couple of cases before continuing to help build that intuition.

If you already have a strong intuition about how React children work then feel
free to [skip to our examples demonstrating how to type various children
patterns that commonly show up in React components](#examples).

Our first case is an element with no children:

```js
<MyComponent />;

// which is the same as...
React.createElement(MyComponent, {});
```

If you pass in no children when creating an element of `MyComponent` then
`props.children` will not be set. If you try to access `props.children`, it
will be undefined.

What happens when you have a single child?

```js
<MyComponent>{42}</MyComponent>;

// which is the same as...
React.createElement(MyComponent, {}, 42);
```

If you pass in a single value then `props.children` will be *exactly* that
single value. Here `props.children` will be the number 42. Importantly,
`props.children` will not be an array! It will be *exactly* the number 42.

What happens when you have multiple children?

```js
<MyComponent>{1}{2}</MyComponent>;

// which is the same as...
React.createElement(MyComponent, {}, 1, 2);
```

Now, if you pass two values then `props.children` will be an array. Specifically
in this case `props.children` will be `[1, 2]`.

Multiple children may also look like:

```js
<MyComponent>{'hello'} world</MyComponent>;

// which is the same as...
React.createElement(MyComponent, {}, 'hello', ' world');
```

In which `props.children` would be the array `['hello', ' world']`.

or:

```js
<MyComponent>{'hello'} <strong>world</strong></MyComponent>;

// which is the same as...
React.createElement(
  MyComponent,
  {},
  'hello',
  ' ',
  React.createElement('strong', {}, 'world'),
);
```

In which `props.children` would be the array
`['hello', ' ', <strong>world</strong>]`.

Moving on to the next case. What happens if we have a single child, but that
child is an array?

```js
<MyComponent>{[1, 2]}</MyComponent>;

// which is the same as...
React.createElement(MyComponent, {}, [1, 2]);
```

This follows the same rule that when you pass in a single child then
`props.children` will be *exactly* that value. Even though `[1, 2]` is an array
it is a single value and so `props.children` will be *exactly* that value. That
is to say `props.children` will be the array `[1, 2]` and not an array of
arrays.

This case happens often when you use `array.map()` such as in:

```js
<MyComponent>
  {messages.map(message => <strong>{message}</strong>)}
</MyComponent>

// which is the same as...
React.createElement(
  MyComponent,
  {},
  messages.map(message => React.createElement('strong', {}, message)),
);
```

So a single array child is left alone, but what happens if we have multiple
children that are arrays?

```js
<MyComponent>{[1, 2]}{[3, 4]}</MyComponent>;

// which is the same as...
React.createElement(MyComponent, {}, [1, 2], [3, 4]);
```

Here `props.children` will be an array of arrays. Specifically `props.children`
will be `[[1, 2], [3, 4]]`.

The rule to remember with React children is that if you have no children then
`props.children` will not be set, if you have one single child then
`props.children` will be set to exactly that value, and if you have two or more
children then `props.children` will be a new array of those values.

> **Note:** Watch out for whitespace! Take the following:
>
> ```
> <MyComponent>{42}  </MyComponent>
> ```
>
> This compiles to: `React.createElement(MyComponent, {}, 42, '  ')`. (With the
> spaces!) See how the spaces show up as part of the children? In this case
> `props.children` would be `[42, '  ']` and *not* the number 42. However, the
> following is fine:
>
> ```
> <MyComponent>
>   {42}
> </MyComponent>
> ```
>
> It will compile to what you would expect:
> `React.createElement(MyComponent, {}, 42)`.
>
> Newlines and indentation after newlines are stripped, but watch out for
> whitespace when using a component with strict types around what children may
> be.

> **Note:** Watch out for comments! Take the following:
>
> ```
> <MyComponent>
>   // some comment...
>   {42}
> </MyComponent>
> ```
>
> This compiles to:
> `React.createElement(MyComponent, {}, '// some comment...', 42)`. See how
> the comment is included in the element's children? In this case
> `props.children` would be `['// some comment...', 42]` which includes the
> comment. To write comments in JSX use the following syntax:
>
> ```
> <MyComponent>
>   {/* some comment... */}
>   {42}
> </MyComponent>
> ```

<p id="examples">
Now let's see how you would take this intuition and type the children of various
React components.
</p>

## Only allowing a specific element type as children. <a class="toc" id="toc-only-allowing-a-specific-element-type-as-children" href="#toc-only-allowing-a-specific-element-type-as-children"></a>

Sometimes you only want a specific component as the children to your React
component. This often happens when you are building a table component which needs
specific column children components, or a tab bar which needs specific
configuration for each tab. One such tab bar component that uses this pattern is
React Native's `<TabBarIOS>` component.

[React Native's `<TabBarIOS>` component][] only allows React element children
and those elements *must* have a component type of `<TabBarIOS.Item>`. You are
expected to use `<TabBarIOS>` like:

[React Native's `<TabBarIOS>` component]: http://facebook.github.io/react-native/docs/tabbarios.html

```js
<TabBarIOS>
  <TabBarIOS.Item>{/* ... */}</TabBarIOS.Item>
  <TabBarIOS.Item>{/* ... */}</TabBarIOS.Item>
  <TabBarIOS.Item>{/* ... */}</TabBarIOS.Item>
</TabBarIOS>
```

You are not allowed to do the following when using `<TabBarIOS>`:

```js
<TabBarIOS>
  <TabBarIOS.Item>{/* ... */}</TabBarIOS.Item>
  <TabBarIOS.Item>{/* ... */}</TabBarIOS.Item>
  <View>{/* ... */}</View>
  <SomeOtherComponent>{/* ... */}</SomeOtherComponent>
  <TabBarIOS.Item>{/* ... */}</TabBarIOS.Item>
</TabBarIOS>
```

See how we added `<View>` and `<SomeOtherComponent>` as children to
`<TabBarIOS>`? This is not allowed and `<TabBarIOS>` will throw an error. How do
we make sure Flow does not allow this pattern?

```js
import * as React from 'react';

class TabBarIOSItem extends React.Component<{}> {
  // implementation...
}

type Props = {
  children: React.ChildrenArray<React.Element<typeof TabBarIOSItem>>,
};

class TabBarIOS extends React.Component<Props> {
  static Item = TabBarIOSItem;
  // implementation...
}

<TabBarIOS>
  <TabBarIOS.Item>{/* ... */}</TabBarIOS.Item>
  <TabBarIOS.Item>{/* ... */}</TabBarIOS.Item>
  <TabBarIOS.Item>{/* ... */}</TabBarIOS.Item>
</TabBarIOS>;
```

We set the type of props to
`React.ChildrenArray<React.Element<typeof TabBarIOSItem>>` which will guarantee
that `<TabBarIOS>` must only have children that are `TabBarIOS.Item` React
elements.

Our [types reference](../types/) has more information about both
[`React.ChildrenArray<T>`](../types/#toc-react-childrenarray) and
[`React.Element<typeof Component>`](../types/#toc-react-element).

> **Note:** If you want methods like `map()` and `forEach()` or to handle a
> [`React.ChildrenArray<T>`](../types/#toc-react-childrenarray) as a normal
> JavaScript array then React provides the [`React.Children` API][] to do just
> this. It has functions like `React.Children.toArray(props.children)` that you
> can use to treat your [`React.ChildrenArray<T>`](../types/#toc-react-childrenarray)
> as a flat array.

[`React.Children` API]: https://facebook.github.io/react/docs/react-api.html#react.children

## Enforcing that a component only gets a single child. <a class="toc" id="toc-enforcing-that-a-component-only-gets-a-single-child" href="#toc-enforcing-that-a-component-only-gets-a-single-child"></a>

Sometimes you want to enforce that your component will *only* receive a single
child. You could use [`React.Children.only()` function][] to enforce this
constraint, but you could also enforce this in Flow. To do this, do NOT
wrap the type for your children in
[`React.ChildrenArray<T>`](../types/#toc-react-childrenarray). Specify a single element argument, like so:

[`React.Children.only()` function]: https://facebook.github.io/react/docs/react-api.html#react.children.only

```js
import * as React from 'react';

type Props = {
  children: React.Element<any>,
};

function MyComponent(props: Props) {
  // implementation...
}

// Not allowed! You must have children.
<MyComponent />;

// Not ok! We have multiple element children.
<MyComponent>
  <div />
  <div />
  <div />
</MyComponent>;

// This is ok. We have a single element child.
<MyComponent>
  <div />
</MyComponent>;
```

## Typing function children or other exotic children types. <a class="toc" id="toc-typing-function-children-or-other-exotic-children-types" href="#toc-typing-function-children-or-other-exotic-children-types"></a>

React allows you to pass *any* value as the children of a React component. There
are some creative uses of this capability such as using a function for children
which could look like this:

```js
<MyComponent>
  {data => (
    <div>{data.foo}</div>
  )}
</MyComponent>
```

`react-router` version 4 asks for a [function as the children to its `<Route>`
component][]. You would provide a function as the children to `react-router`
like this:

[function as the children to its `<Route>` component]: https://reacttraining.com/react-router/core/api/Route/children-func

```js
<Route path={to}>
  {({ match }) => (
    <li className={match ? 'active' : ''}>
      <Link to={to} {...rest}/>
    </li>
  )}
</Route>
```

(Example adapted from the [`react-router` documentation][].)

[`react-router` documentation]: https://reacttraining.com/react-router/core/api/Route/children-func

Here is how you would type the `<Route>` component in Flow:

```js
import * as React from 'react';

type Props = {
  children: (data: { match: boolean }) => React.Node,
  path: string,
  // other props...
};

class Route extends React.Component<Props> {
  // implementation...
}

<Route path={to}>
  {({ match }) => (
    <li className={match ? 'active' : ''}>
      <Link to={to} {...rest}/>
    </li>
  )}
</Route>;
```

The type for `children` is a function that takes in some object type and returns
a [`React.Node`](../types/#toc-react-node) which is the type for any value
that can be rendered by React. A `children` function does not need to return
[`React.Node`](../types/#toc-react-node). It could return any type, but in this
case `react-router` wants to render the result returned by the `children`
function.

This pattern is also not limited to function children. You could also pass in
arbitrary object or class types.

# Using `React.Node` but without some primitive types like strings. <a class="toc" id="toc-using-react-node-but-without-some-primitive-types-like-strings" href="#toc-using-react-node-but-without-some-primitive-types-like-strings"></a>

[`React.Node`](../types/#toc-react-node) is the general type for children, but
sometimes you might want to use [`React.Node`](../types/#toc-react-node) while
excluding some primitives like strings and numbers. [The React Native `<View>`
component][] does this, for example.

[The React Native `<View>` component][] will allow any primitive value or any
React element as its children. However, `<View>` does not allow strings or
numbers as children! You could use [`React.Node`](../types/#toc-react-node) as
the children type for `<View>`, however [`React.Node`](../types/#toc-react-node)
includes strings which we don't want for `<View>`. So we need to create our own
type.

[The React Native `<View>` component]: http://facebook.github.io/react-native/docs/view.html

```js
import * as React from 'react';

type ReactNodeWithoutStrings = React.ChildrenArray<
  | void
  | null
  | boolean
  | React.Element<any>
>;

type Props = {
  children?: ReactNodeWithoutStrings,
  // other props...
};

class View extends React.Component<Props> {
  // implementation...
}
```

[`React.ChildrenArray<T>`](../types/#toc-react-childrenarray) is a type that
models React nested array data structure for children. `ReactNodeWithoutStrings`
uses [`React.ChildrenArray<T>`](../types/#toc-react-childrenarray) to be an
arbitrarily nested array of null, boolean, or React elements.

[`React.Element<typeof Component>`](../types/#toc-react-element) is the type of
a React element like `<div/>` or `<MyComponent/>`. Notably elements are not the
same as components!

> **Note:** If you want methods like `map()` and `forEach()` or to handle a
> [`React.ChildrenArray<T>`](../types/#toc-react-childrenarray) as a normal
> JavaScript array then React provides the [`React.Children` API][] to do just
> this. It has functions like `React.Children.toArray(props.children)` that you
> can use to treat your [`React.ChildrenArray<T>`](../types/#toc-react-childrenarray)
> as a flat array.

[`React.Children` API]: https://facebook.github.io/react/docs/react-api.html#react.children
