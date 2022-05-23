---
title: Announcing Flow Comments
short-title: Flow Comments
author: Gabe Levi
hide_table_of_contents: true
---

As of Flow 0.4.0, you can put your Flow-specific syntax in special comments. If you use these special comments then you do not need to transform away Flow-specific syntax before running your code. While we strongly recommend that you write your code without the special comments, this feature will help people who can't fit a Flow-stripping transformation into their setup. This was one of our [most requested features](https://github.com/facebook/flow/issues/3) and hopefully it will enable even more people to use Flow!

This feature introduces 3 special comments: `/*:`, `/*::`, and `/*flow-include`. Flow will read the code inside these special comments and treat the code as if the special comment tokens didn't exist. These special comments are valid JavaScript block comments, so your JavaScript engine will ignore the code inside the comments.

<!--truncate-->

## The Flow Comment Syntax

There are 3 special comments that Flow currently supports. You may recognize this syntax from [Jarno Rantanen](https://github.com/jareware)'s excellent project, [flotate](https://github.com/jareware/flotate).

### 1. `/*:`

`/*: <your code> */` is interpreted by Flow as `: <your code>`

```JavaScript
function foo(x/*: number*/)/* : string */ { ... }
```

is interpreted by Flow as

```JavaScript
function foo(x: number): string { ... }
```

but appears to the JavaScript engine (ignoring comments) as

```JavaScript
function foo(x) { ... }
```

### 2. `/*::`

`/*:: <your code> */` is interpreted by Flow as `<your code>`

```JavaScript
/*:: type foo = number; */
```

is interpreted by Flow as

```JavaScript
type foo = number;
```

but appears to the runtime (ignoring comments) as

```JavaScript

```

### 3. `/*flow-include`

`/*flow-include <your code> */` is interpreted by Flow as `<your code>`. It behaves the same as `/*::`

```JavaScript
/*flow-include type foo = number; */
```

is interpreted by Flow as

```JavaScript
type foo = number;
```

but appears to the runtime (ignoring comments) as

```JavaScript

```

Note: whitespace is ignored after the `/*` but before the `:`, `::`, or `flow-include`. So you can write things like

```JavaScript
/* : number */
/* :: type foo = number */
/* flow-include type foo = number */
```

## Future Work

We plan to update our Flow transformation to wrap Flow syntax with these special comments, rather than stripping it away completely. This will help people write Flow code but publish code that works with or without Flow.

## Thanks

Special thanks to [Jarno Rantanen](https://github.com/jareware) for building [flotate](https://github.com/jareware/flotate) and supporting us merging his syntax upstream into Flow.
