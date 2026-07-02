---
title: .flowconfig [libs]
slug: /config/libs
description: "Configure which library definition files Flow should use when type checking your code."
---

The `[libs]` section in a `.flowconfig` file tells Flow to include the
specified [library definitions](../libdefs/index.md) when type
checking your code. Multiple libraries can be specified. By default, the
`flow-typed` folder in your project root directory is included as a library
directory. This default allows you to use
[`flow-typed`](https://github.com/flow-typed/flow-typed) to install library
definitions without additional configuration.

Each line in the `[libs]` section is a path to the library file or directory
which you would like to include. These paths can be relative to the project
root directory or absolute. Including a directory recursively includes all the
files under that directory as library files.

## See Also {#toc-see-also}

- [Library Definitions](../libdefs/index.md) — what library definitions are and how to use them
- [Creating Library Definitions](../libdefs/creation.md) — writing your own library definitions
