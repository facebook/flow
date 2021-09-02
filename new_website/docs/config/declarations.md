---
title: .flowconfig [declarations]
slug: /config/declarations
---

Often third-party libraries have broken type definitions or have type
definitions only compatible with a certain version of Flow. In those cases it
may be useful to use type information from the third-party libraries without
typechecking their contents.

### `[declarations]` {#toc-declarations}

The `[declarations]` section in a `.flowconfig` file tells Flow to parse files
matching the specified regular expressions in _declaration mode_. In declaration
mode the code is not typechecked. However, the signatures of functions, classes,
etc are extracted and used by the typechecker when checking other code.

Conceptually one can think of declaration mode as if Flow still typechecks the
files but acts as if there is a comment that matches
[`suppress_comment`](./options/#toc-suppress-comment-regex) on every line.

See also `[untyped]`(untyped) for not typechecking files, and instead using `any` for all contents.

Things to keep in mind:

1.  Declaration mode should only be used for existing third-party code. You
    should never use this for code under your control.
2.  These are [OCaml regular expressions](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html#TYPEregexp).
3.  These regular expressions match against absolute paths. They probably should
    start with `.*`

An example `[declarations]` section might look like:

```
[declarations]
.*/third_party/.*
.*/src/\(foo\|bar\)/.*
.*\.decl\.js
```

This `[declarations]` section will parse in declaration mode:

1.  Any file or directory under a directory named `third_party`
2.  Any file or directory under `.*/src/foo` or under `.*/src/bar`
3.  Any file that ends with the extension `.decl.js`

Starting with Flow v0.23.0, you may use the `<PROJECT_ROOT>` placeholder in
your regular expressions. At runtime, Flow will treat the placeholder as if it
were the absolute path to the project's root directory. This is useful for
writing regular expressions that are relative rather than absolute.

For example, you can write:

```
[declarations]
<PROJECT_ROOT>/third_party/.*
```

Which would parse in declaration mode any file or directory under the directory
named `third_party/` within the project root. However, unlike the previous
example's `.*/third_party/.*`, it would NOT parse files or directories under
directories named `third_party/`, like `src/third_party/`.
