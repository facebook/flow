---
title: .flowconfig
slug: /config
description: Flow tries to work out of the box as much as possible, but can be configured to work with any codebase.
---

Every Flow project contains a `.flowconfig` file. You can configure Flow by
modifying `.flowconfig`. New projects or projects that are starting to use Flow
can generate a default `.flowconfig` by running `flow init`.

### `.flowconfig` format {#toc-flowconfig-format}

The `.flowconfig` uses a custom format that vaguely resembles INI files.

The `.flowconfig` consists of different sections:

* [`[version]`](./version.md)
* [`[options]`](./options.md)
* [`[include]`](./include.md)
* [`[ignore]`](./ignore.md)
* [`[untyped]`](./untyped.md)
* [`[declarations]`](./declarations.md)
* [`[libs]`](./libs.md)
* [`[lints]`](./lints.md)
* [`[strict]`](../strict/index.md#toc-enabling-flow-strict-in-a-flowconfig)

### Comments {#toc-comments}

Lines beginning with zero or more spaces followed by an `#` or `;` or `💩` are
ignored. For example:

```
# This is a comment
  # This is a comment
; This is a comment
  ; This is a comment
💩 This is a comment
  💩 This is a comment
```

### Where to put the `.flowconfig` {#toc-where-to-put-the-flowconfig}

The location of the `.flowconfig` is significant. Flow treats the directory that
contains the `.flowconfig` as the _project root_. By default Flow includes all
the source code under the project root. The paths in the
[[include] section](./include.md) are relative to the project root. Some other
configuration also lets you reference the project root via the macro
`<PROJECT_ROOT>`.

Most people put the `.flowconfig` in the root of their project (i.e. next to the
`package.json`). Some people put all their code in a `src/` directory and
therefore put the `.flowconfig` at `src/.flowconfig`.

### Example {#toc-example}

Say you have the following directory structure, with your `.flowconfig` in
`mydir`:

```text
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

```text
[include]
../otherdir/src

[ignore]
.*/build/.*

[libs]
./lib
```

Now `flow` will include a directory outside the `.flowconfig` path in its
check, ignore the `build` directory and use the declarations in `lib`.
