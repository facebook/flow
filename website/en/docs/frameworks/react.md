---
layout: guide
---

Flow supports JSX and has done a lot of work internally to work great with
React applications.

## Setup Flow with React <a class="toc" id="toc-setup-flow-with-react" href="#toc-setup-flow-with-react"></a>

Flow and Babel work well together, so it doesn't take much to adopt as a React
user. If you need to setup Babel with Flow, you can follow
[this guide](../../tools/babel/).

Babel also
[works out of the box with Create React App](../../tools/create-react-app),
just install Flow and create a `.flowconfig`.

## Typing React with Flow <a class="toc" id="toc-typing-react-with-flow" href="#toc-typing-react-with-flow"></a>

React has been designed with Flow in mind, so most things take very little
effort to type properly.

### Adding types for React Component `props` <a class="toc" id="toc-adding-types-for-react-component-props" href="#toc-adding-types-for-react-component-props"></a>

You can replace all of your `propTypes` with a simple `props:` field in your
component classes.

You can also specify `defaultProps` as a static class property and Flow's
inference will handle the rest for you.

```js
class MyComponent extends React.Component {
  props: {
    prop1: string,
    prop2: number,
  };

  static defaultProps = {
    prop1: "foo"
  };
}
```

> **Note:** You don't have to mark your `defaultProps` as optional properties
> in your `props`. Flow knows how to handle them properly.

If you cannot use a static class property for `defaultProps`, you can also add
a class field annotation and assign `defaultProps` after the class declaration.

```js
class MyComponent extends React.Component {
  static defaultProps: {
    prop1: string
  };
}

MyComponent.defaultProps = {
  prop1: "foo"
};
```

### Adding types for React Component `state` <a class="toc" id="toc-adding-types-for-react-component-state" href="#toc-adding-types-for-react-component-state"></a>

If you specify `state` as a class property you can rely on inference to type
it correctly.

```js
class MyComponent extends React.Component {
  state = {
    foo: 1,
    bar: true,
    baz: 'three',
  };
}
```

If you cannot use a class property for `state`, you can also add a class field
annotation and assign it in the constructor.

```js
class MyComponent extends React.Component {
  state: {
    foo: number,
    bar: boolean,
    baz: string,
  };

  constructor(props) {
    super(props);
    this.state = {
      foo: 1,
      bar: true,
      baz: 'three',
    };
  }
}
```

### Explicitly specifying React Component generics <a class="toc" id="toc-explicitly-specifying-react-component-generics" href="#toc-explicitly-specifying-react-component-generics"></a>

It can sometimes be useful to explicitly specify the generics of
`React.Component`. You can do this by creating type aliases for your
`defualtProps`, `props`, and `state` and passing them as generics to
`React.Component` in that order.

```js
type DefaultProps = { prop: string };
type Props        = { prop: string };
type State        = { prop: string };

class MyComponent extends React.Component<DefaultProps, Props, State> {
  static defaultProps = { prop: "foo" };
  state = { prop: "bar" };
}
```

### Adding types for React events <a class="toc" id="toc-adding-types-for-react-events" href="#toc-adding-types-for-react-events"></a>

```js
class MyComponent extends React.Component {
  onEvent(event: Event) {
    // ...
  }
}
```

If you want a more specific type of event you can use one of the
[derived classes](https://developer.mozilla.org/en-US/docs/Web/API/Event#Introduction)
such as `MouseEvent`.

```js
class MyComponent extends React.Component {
  onMouseEvent(event: MouseEvent) {
    // ...
  }
}
```

If you want to add the type of the element you can do so with an intersection
type specifying the `currentTarget`.

```js
class MyComponent extends React.Component {
  onButtonEvent(event: Event & { currentTarget: HTMLButtonElement }) {
    // ...
  }
}
```

> **Note:** You should prefer `currentTarget` over `target` as `target` could
> be any nested element due to [DOM event propagation](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Examples#Example_5:_Event_Propagation).

### Adding types for React refs <a class="toc" id="toc-adding-types-for-react-refs" href="#toc-adding-types-for-react-refs"></a>

All you need to do to type a ref is add a matching class field with the
[element type](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model#HTML_element_interfaces) (such as `HTMLElement` or `HTMLButtonElement`).

```js
import React from 'react';

class MyComponent extends React.Component {
  button: HTMLButtonElement;

  render() {
    return <button ref={el => this.button = el}>Toggle</button>;
  }
}
```

### Adding types to React lifecycle methods <a class="toc" id="toc-adding-types-to-react-lifecycle-methods" href="#toc-adding-types-to-react-lifecycle-methods"></a>

React component lifecycle methods that recieve `props` and `state` should be
typed with the same types as you have provided for the component.

```js
type Props = { /* ... */ };
type State = { /* ... */ };

class MyComponent extends React.Component<void, Props, State> {
  componentDidUpdate(prevProps: Props, prevState: State) {
    // ...
  }
}
```

### Adding types to React functional components <a class="toc" id="toc-adding-types-to-react-functional-components" href="#toc-adding-types-to-react-functional-components"></a>

React's [functional components](https://facebook.github.io/react/docs/components-and-props.html#functional-and-class-components)
can be typed the same way any function can be typed in Flow.

```js
// @flow
let MyComponent = (props: { foo: string }) => {
  return <div>{props.foo}</div>
};

let a = <MyComponent/>;           // Error!
let b = <MyComponent foo="bar"/>; // Works!
```

Building on top of [object destructuring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment#Object_destructuring)
these functional components can specify default props with default values.

```js
let MyComponent = ({ foo, bar = 2 }: { foo: number, bar?: number }) => {
  return <div>{foo + bar}</div>;
};

let a = <MyComponent/>;                     // Error!
let b = <MyComponent foo={42}/>;            // Works!
let c = <MyComponent foo={42} bar={3.14}/>; // Works!
```
