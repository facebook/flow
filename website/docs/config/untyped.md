---
title: .flowconfig [untyped]
slug: /config/untyped
---

Flow needs to know which files to parse and of those how to handle any Flow types within them. For third party libraries you may wish to not parse, parse but not preserve types, or parse but not raise errors, depending on the quality and compatibility of their Flow implementation.

Different sections are available to specify different behaviours, and by combining them most use cases are expected to be met.

### `[untyped]` {#toc-untyped}

The `[untyped]` section in a `.flowconfig` file tells Flow to not typecheck files
matching the specified regular expressions and instead throw away types and treat modules as `any`.

This is different from the `[ignore]` config section that causes matching files to be ignored by the module resolver, which inherently makes them un-typechecked, and also unresolvable by `import` or `require`. When ignored `[libs]` must then be specified for each `import` using `flow-typed`, which may not always be desired.

It is also different from the `[declarations]` section. This also does not typecheck the file contents, but `[declarations]` does extract and use the signatures of functions, classes, etc, when checking other code.

`[untyped]` instead causes a file to be ignored by the typechecker as if it had `noflow` in it, resolve modules as `any` type, but allow them to NOT be ignored by the module resolver. Any matching file is skipped by Flow (not even parsed, like other `noflow` files!), but can still be `require()`'d.

Things to keep in mind:

1.  These are [OCaml regular expressions](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html#TYPEregexp).
2.  These regular expressions match against absolute paths. They probably should
    start with `.*`

An example `[untyped]` section might look like:

```
[untyped]
.*/third_party/.*
.*/src/\(foo\|bar\)/.*
.*\.untype\.js
```

This `[untyped]` section will parse:

1.  Any file or directory under a directory named `third_party`
2.  Any file or directory under `.*/src/foo` or under `.*/src/bar`
3.  Any file that ends with the extension `.untype.js`

Starting with Flow v0.23.0, you may use the `<PROJECT_ROOT>` placeholder in
your regular expressions. At runtime, Flow will treat the placeholder as if it
were the absolute path to the project's root directory. This is useful for
writing regular expressions that are relative rather than absolute.

For example, you can write:

```
[untyped]
<PROJECT_ROOT>/third_party/.*
```

Which would parse in declaration mode any file or directory under the directory
named `third_party/` within the project root. However, unlike the previous
example's `.*/third_party/.*`, it would NOT parse files or directories under
directories named `third_party/`, like `src/third_party/`.


