---
id: advanced-configuration
title: Advanced Configuration
layout: docs
permalink: /docs/advanced-configuration.html
prev: troubleshooting.html
next: react-example.html
---

This section discusses some of the advanced configuration options available 
for customizing how `flow` runs.

## `.flowconfig`

Many times it is enough to create an empty `.flowconfig` file via `flow init` 
and just have `.flowconfig` simply be the token that tells flow to "start type 
checking here".

However, `.flowconfig` does provide some configuration options that can be 
used to customize what files `flow` accesses and what files it ignores.

### `[include]`

The `[include]` heading in a `.flowconfig` file tells `flow` to include the 
specified files or directories when type checking your code.

### `[ignore]`

Conversely, the `[ignore]` heading in a `.flowconfig` file tells `flow` to ignore files matching the specified regular expressions when type checking your code.

> WARNING
> 
> The specified regular expressions in the [ignore] section apply to absolute paths, 
> so make sure you use repeated wildcards `.*` at the beginning if you're specifying relative paths. 
> Also, relative paths that begin with `.` or `..` won't work, since the regular expression parser
> would confuse them with wildcards.

### `[libs]`

The `[libs]` heading in a `.flowconfig` file tells `flow` to include the 
specified [declarations](http://flowtype.org/docs/declarations.html) when type checking your code. Multiple libraries can be specified.

### `[options]`

The `[options]` heading in a `.flowconfig` file tells `flow` lets you specify the module system via `module.system=haste` or `module.system=node`.

### Example

Say you have the following directory structure, with your `.flowconfig` in 
`mydir`:

```bbcode
otherdir
└── src
    ├── othercode.js 
mydir
├── .flowconfig
├── build
│   ├── first.js
│   └── shim.js
├── lib
│   └── flow
├── node_modules
│   └── es6-shim
└── src
    ├── first.js
    └── shim.js
```

Here is an example of how you could use the `.flowconfig` directives. 

```bbcode
[include]
/home/otherdir/src

[ignore]
.*/build/.*

[libs]
./lib
```

Now `flow` will include a directory outside the `.flowconfig` path in its 
check, ignore the `build` directory and use the declarations in  `lib`.
