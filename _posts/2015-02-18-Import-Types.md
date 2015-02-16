---
author: jeffmo 
---

### Announcing Import Type

As of Flow 0.3.0 (which went out last week), it's now possible to import the type of a class from another module. More specifically: If you're only importing a class for purposes of referencing it in a type annotation, you can use the new `import type` syntax to do this.

### Motivation

Has this ever happened to you:

```JavaScript
// Post-transformation lint error: Unused variable 'URI'
var URI = require('URI');

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
import type * as URI from 'URI';
module.exports = function(x: URI): URI { 
  return x; 
};
```

If you have a module that exports multiple classes (like, say, a Crayon and a Marker class), you can import the type for each of them together or separately like this:

```JavaScript
import type {Crayon, Marker} from 'WritingUtensils';
module.exports = function junkDrawer(x: Crayon, y: Marker): void {}
```

### Transformations

Like type annotations and other Flow features, `import type` need to be transformed away before the code can be run. The transforms will be available in react-tools `0.13.0` when it is published soon, but for now they're available in `0.13.0-beta.2`, which you can install with 

```bash
npm install react-tools@0.13.0-beta.2
```

### Anticipatory Q&A

#### **Wait, but what happens at runtime after I've added an `import type` declaration?**
*Nothing! All `import type` declarations get stripped away just like other flow syntax.*

#### **Can I use `import type` to pull in type aliases from another module, too?**
*Not quite yet...but soon! There are a few other moving parts that we need to build first, but we're working on it.*

