---
layout: guide
---

Empty type has no values, it is also known as [bottom type](https://en.wikipedia.org/wiki/Bottom_type).

The `empty` type is a subtype of _every_ type.

For example, `empty` type is the return type of a function that throws exception:

```js
// @flow
function error(message: string): empty {
  throw new Error(message)
}
```

Functions that return `empty` are called _diverging functions_.

You can use `empty`-assertions to describe impossible branches in your code.

For example:

```js
//@flow
type User = {type: 'userSelf'} | {type: 'userForeign', firstName: string}

function handleUser(data: User): string {
  switch (data.type) {
    case 'userSelf':
      return 'Me'
    case 'userForeign':
      return user.firstName
    default:
      ;(data.type: empty) // This would error if you forgot to handle case, or misspelled type
  }
}
```
