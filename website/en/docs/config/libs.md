---
layout: guide
---

The `[libs]` section in a `.flowconfig` file tells Flow to include the
specified [library definitions](../../libdefs/) when type
checking your code. Multiple libraries can be specified. By default, the
`flow-typed` folder in your project root directory is included as a library
directory. This default allows you to use
[`flow-typed`](https://github.com/flowtype/flow-typed) to install library
definitions without additional configuration.

Each line in the `[libs]` section is a path to the library file or directory
which you would like to include. These paths can be relative to the project
root directory or absolute. Including a directory recursively includes all the
files under that directory as library files.
