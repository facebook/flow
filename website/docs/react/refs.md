---
title: Ref Functions
slug: /react/refs
---

React allows you to grab the instance of an element or component with [`ref`
functions][]. To use a ref function add a [maybe instance type][] to your class
and assign your instance to that property in your ref function.

[`ref` functions]: https://facebook.github.io/react/docs/refs-and-the-dom.html
[maybe instance type]: ../../types/maybe/

```js
import * as React from 'react';

class MyComponent extends React.Component<{}> {
  // The `?` here is important because you may not always have the instance.
  button: ?HTMLButtonElement;

  render() {
    return <button ref={button => (this.button = button)}>Toggle</button>;
  }
}
```

The `?` in `?HTMLButtonElement` is important. In the example above
the first argument to `ref` will be `HTMLButtonElement | null` as React will
[call your `ref` callback with null][] when the component unmounts. Also, the
`button` property on `MyComponent` will not be set until React has finished
rendering. Until then your `button` ref will be undefined. Protect yourself
against these cases and use a `?` (like in `?HTMLButtonElement`) to protect
yourself from bugs.

[call your `ref` callback with null]: https://facebook.github.io/react/docs/refs-and-the-dom.html#adding-a-ref-to-a-dom-element
