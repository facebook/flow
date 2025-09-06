---
title: Ref Functions
slug: /react/refs
---

React allows you to grab the instance of an element or component with [refs](https://react.dev/learn/manipulating-the-dom-with-refs).

##  Refs in Functional Components {#toc-refs-in-functional-components}

Inside a functional component, refs are accessed with the `useRef` hook:

```js flow-check
import {useRef} from 'react';
import * as React from 'react';

function MyComponent() {
  const buttonRef = useRef<null | HTMLButtonElement>(null);
  buttonRef as {current: null | HTMLButtonElement}; // useRef wraps the ref value in an object
  return <button ref={buttonRef}>Toggle</button>;
}
```

Note that `useRef` wraps the ref value in an object with a `current` property. This must be
reflected in the type of anything accepting the ref value.

##  Refs in Class Components {#toc-refs-in-class-components}

Refs in class components are similar to function components. To create one, add a
property to your class and assign the result of `React.createRef` to it.

```js flow-check
import * as React from 'react';

class MyComponent extends React.Component<{}> {
  // The `null` here is important because you may not always have the instance.
  buttonRef: {current: null | HTMLButtonElement};

  constructor() {
    super();
    this.buttonRef = React.createRef<HTMLButtonElement>();
  }

  render(): React.Node {
    return <button ref={this.buttonRef}>Toggle</button>;
  }
}
```

One notable difference between `useRef` and `createRef` is that `createRef` does not accept
a default value. It will initialize the ref with the value `null`. This is because
DOM elements will not exist until the first render of `MyComponent` and so a `null` value
must be used.

Again, note that the ref value is wrapped in an object with a `current` property.
