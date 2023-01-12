---
title: Context
slug: /react/context
---

Flow can typecheck your React components that use the [context API][] introduced
in React 16.3.

[context API]: https://reactjs.org/docs/context.html

> **Note:** Typing context values requires Flow 0.70 or later.

Flow will infer types from the way you use a context's `{ Provider, Consumer }`
pair:

```js
import React from "react";

const Theme = React.createContext();

<Theme.Provider value="light" />;

<Theme.Consumer>{value => value.toUpperCase()}</Theme.Consumer>; // Error! `value` is nullable.

<Theme.Consumer>
  {value =>
    value
      ? value.toUpperCase() // Valid: `value` is a string.
      : null
  }
</Theme.Consumer>;
```

If your context has a default value, Flow will type your consumer component
accordingly:

```js
import React from "react";

const Theme = React.createContext("light");

<Theme.Consumer>{value => value.toUpperCase()}</Theme.Consumer>; // Valid: `value` is a non-nullable string.
```

To explicitly specify the type of a context value, pass a type parameter to
`createContext`:

```js
import React from "react";

const Theme = React.createContext<"light" | "dark">("light");

<Theme.Provider value="blue" />; // Error! "blue" is not one of the allowed values.
```
