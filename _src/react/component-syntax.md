---
title: Component Syntax
slug: /react/component-syntax
---

[Components](https://react.dev/learn/your-first-component) are the foundation for building UIs in React. While components are typically expressed using JavaScript functions, Component Syntax provides component primitive values that provide several advantages over function components, like:

1. More elegant syntax with significantly less verbosity and boilerplate than functions
2. Type system support tailored specifically for writing React
3. Better support for [React refs](https://react.dev/learn/manipulating-the-dom-with-refs)

## Basic Usage
You can declare a component with Component Syntax similar to how you'd declare a function:

```js flow-check
import * as React from 'react';

component Introduction(name: string, age: number) {
  return <h1>My name is {name} and I am {age} years old</h1>
}
```

You can use a component directly in JSX: `<Introduction age={9} name="Mr. Flow" />`.

There are a few important details to notice here:

1. the prop parameter names declared in the Introduction component are the same as the prop names passed to Introduction in JSX
2. the order of the parameters in the declaration does not need to match the order that they are provided in JSX

## Parameters

### String Parameters/Renaming Parameters

Components also allow you to rename parameters, which is useful when your parameter name is not a valid JavaScript identifier:

```js flow-check
import * as React from 'react';

component RenamedParameter(
  'required-renamed' as foo: number,
  'optional-renamed' as bar?: number,
  'optional-with-default-renamed' as baz?: number = 3,
) {
  (foo: number); // OK
  (bar: number | void); // OK
  (baz: number); // OK

  return <div />;
}
```

### Rest Parameters

Sometimes you do not want to list out every prop explicitly because you do not intend to reference them individually in your component. This is common when you are writing a component that wraps another and need to pass props from your component to the inner one:

```jsx
import * as React from 'react';

import type {Props as StarProps} from './Star';
import Star from './Star';

component BlueStar(...props: StarProps) {
  return <Star {...props} color="blue" />;
}
```

Rest parameters use an object type as an annotation, which means you can use existing type utilities like object spreads and Pick to annotate more complex prop patterns:

```js flow-check
import * as React from 'react';

component OtherComponent(foo: string, bar: number) {
  return foo + bar;
}

component FancyProps(
  ...props: {
    ...React.PropsOf<OtherComponent>,
    additionalProp: string,
  }
) {
  return <OtherComponent foo={props.foo} bar={props.bar} />;
}
```

### Optional Parameters and Defaults

Components allow you to declare optional parameters and specify defaults:

```js flow-check
import * as React from 'react';

component OptionalAndDefaults(
  color: string = "blue",
  extraMessage?: string,
) {
  let message = `My favorite color is ${color}.`;
  if (extraMessage != null) {
    message += `\n${extraMessage}`;
  }
  return <p>{message}</p>
}

<OptionalAndDefaults /> // No error, all of the parameters are optional!
```

### Destructuring Parameters

The `as` operator also allows you to destructure your parameters:

```js flow-check
import * as React from 'react';

component Destructuring(
  config as {color, height}: $ReadOnly<{color: number, height: number}>,
) { return <div /> }
```

Rest parameters can be destructured without using as:

```js flow-check
import * as React from 'react';

type Props = $ReadOnly<{ color: string, height: number }>;

component DestructuredRest(...{color, height}: Props) { return <div /> }
```

### Ref Parameters

To access refs in components you just need to add a ref parameter.

```js flow-check
import * as React from 'react';

component ComponentWithARef(ref: React.RefSetter<HTMLElement>) {
  return <div ref={ref} />;
}
```

Behind the scenes Component Syntax will wrap the component in the required [React.forwardRef call](https://react.dev/reference/react/forwardRef) to ensure the component works as expected at runtime. The one restriction for refs is they must be defined as an inline parameter, refs within rest params are not supported. This is due to the need to compile in the `forwardRef` call, for this to work correctly we need to be able to statically determine the ref from the component definition.

## Rules for Components

Component Syntax enforces a few restrictions in components to help ensure correctness:

1. The return values must be a subtype of `React.Node`, otherwise React may crash while rendering your component.
2. All branches of a component must end in an explicit return. Even though `undefined` is a valid return value, we've seen many instances where an explicit return would have prevented bugs in production.
3. You cannot use `this` in a component.

So these components are invalid:

```js flow-check
import * as React from 'react';

component InvalidReturnValue() {
  return new Object(); // ERROR: Value does not match `React.Node` type
}

component ImplicitReturn(someCond: boolean) {
  if (someCond) {
    return <h1>Hello World!</h1>;
  }
  // ERROR: No return in this branch
}

component UsesThis() {
  this.foo = 3; // ERROR: Accessing `this`
  return null;
}
```

## Enable Component Syntax {#toc-enable-component-syntax}

In your `.flowconfig`, under the `[options]` heading, add `component_syntax=true`.
