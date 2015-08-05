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

The `[options]` heading in a `.flowconfig` file can contain several key-value pairs:

- `log.file` (string): the path to the log file (defaults to `/tmp/flow/<escaped root path>.log`)
- `module.name_mapper` (regex -> string): specify a regular expression to match against module names, and a replacement value, separated by a `->`.

    For example:
    
    ```
    module.name_mapper= '^image![a-zA-Z0-9$_]+$' -> 'ImageStub'
    ```
  
    makes Flow treat `require('image!foo.jpg')` as if it was `require('ImageStub')`.
  
    (**note:** you can specify `module.name_mapper` multiple times)

- `module.system` (`node` | `haste`): the module system to use to resolve `import` and `require`

- `munge_underscores` (boolean): set this to `true` to have Flow treat underscore-prefixed class properties and methods as private. This should be used in conjunction with [`jstransform`'s ES6 class transform](https://github.com/facebook/jstransform/blob/master/visitors/es6-class-visitors.js), which enforces the same privacy at runtime.

- `traces` (integer): enables traces on all error output (showing additional details about the flow of types through the system), to the depth specified. This can be very expensive, so is disabled by default.

- `strip_root` (boolean): set this to `true` to always strip the root directory from file paths in error messages

- `suppress_comment` (regex): defines a magical comment that suppresses any Flow errors on the following line. For example:
    
    ```
    suppress_comment= \\(.\\|\n\\)*\\$FlowFixMe
    ```
    
    will match a comment like this:
  
    ```
    // $FlowFixMe: suppressing this error until we can refactor
    var x : string = 123;
    ```
    
    and suppress the error. If there is no error on the next line (the suppresion is unnecessary), an "Unused suppression" error will be shown instead.
  
    **Note:** you can specify `suppress_comment` multiple times. We recommend defining something like `$FlowFixMe` (for type errors that need to be fixed) in addition to `$FlowIssue` (to suppress errors caused by bugs in Flow).

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
