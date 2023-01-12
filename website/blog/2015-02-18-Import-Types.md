---
title: Announcing Import Type
short-title: Import Type
author: Jeff Morrison
hide_table_of_contents: true
---

As of Flow 0.3.0, it's now possible to import types from another module. So, for example, if you're only importing a class for purposes of referencing it in a type annotation, you can now use the new `import type` syntax to do this.

## Motivation

Has this ever happened to you:

```JavaScript
// @flow

// Post-transformation lint error: Unused variable 'URI'
import URI from "URI";

// But if you delete the require you get a Flow error:
// identifier URI - Unknown global name
module.exports = function(x: URI): URI {
  return x;
}
```

Now you have an out! To solve this problem (and with an eye toward a near future with ES6 module syntax), we've added the new `import type` syntax.  With `import type`, you can convey what you really mean here — that you want to import the *type* of the class and not really the class itself.

<!--truncate-->

## Enter Import Type

So instead of the above code, you can now write this:

```JavaScript
// @flow

import type URI from 'URI';
module.exports = function(x: URI): URI {
  return x;
};
```

If you have a module that exports multiple classes (like, say, a Crayon and a Marker class), you can import the type for each of them together or separately like this:

```JavaScript
// @flow

import type {Crayon, Marker} from 'WritingUtensils';
module.exports = function junkDrawer(x: Crayon, y: Marker): void {}
```

## Transformations

Like type annotations and other Flow features, `import type` need to be transformed away before the code can be run. The transforms will be available in react-tools `0.13.0` when it is published soon, but for now they're available in `0.13.0-beta.2`, which you can install with

```bash
npm install react-tools@0.13.0-beta.2
```

## Anticipatory Q&A

### Wait, but what happens at runtime after I've added an `import type` declaration?
*Nothing! All `import type` declarations get stripped away just like other flow syntax.*

### Can I use `import type` to pull in type aliases from another module, too?
<del>Not quite yet...but soon! There are a few other moving parts that we need to build first, but we're working on it.</del>

EDIT: Yes! As of Flow 0.10 you can use the `export type MyType = ... ;` syntax to compliment the `import type` syntax. Here's a trivial example:

```javascript
// @flow

// MyTypes.js
export type UserID = number;
export type User = {
  id: UserID,
  firstName: string,
  lastName: string
};
```

```javascript
// @flow

// User.js
import type {UserID, User} from "MyTypes";

function getUserID(user: User): UserID {
  return user.id;
}
```

Note that we only support the explicit named-export statements for now (i.e. `export type UserID = number;`). In a future version we can add support for latent named-export statements (i.e. `type UserID = number; export {UserID};`) and default type exports (i.e.  `export default type MyType = ... ;`)...but for now these forms aren't yet supported for type exports.
