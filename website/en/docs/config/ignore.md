---
layout: guide
---

Flow needs to know which files to read and watch for changes. This set of files
is determined by taking all included files and excluding all the ignored files.

### `[ignore]` <a class="toc" id="toc-ignore" href="#toc-ignore"></a>

The `[ignore]` section in a `.flowconfig` file tells Flow to ignore files
matching the specified regular expressions when type checking your code. By
default, nothing is ignored.

Things to keep in mind:

1. These are [OCaml regular   expressions](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html#TYPEregexp).
2. These regular expressions match against absolute paths. They probably should
   start with `.*`
3. Ignores are processed AFTER includes. If you both include and ignore a file
   it will be ignored.

An example `[ignore]` section might look like:

```
[ignore]
.*/__tests__/.*
.*/src/\(foo\|bar\)/.*
.*\.ignore\.js
```

This `[ignore]` section will ignore:

1. Any file or directory under a directory named `__tests__`
2. Any file or directory under `.*/src/foo` or under `.*/src/bar`
3. Any file that ends with the extension `.ignore.js`

Starting with Flow v0.23.0, you may use the `<PROJECT_ROOT>` placeholder in
your regular expressions. At runtime, Flow will treat the placeholder as if it
were the absolute path to the project's root directory. This is useful for
writing regular expressions that are relative rather than absolute.

For example, you can write:

```
[ignore]
<PROJECT_ROOT>/__tests__/.*
```

Which would ignore any file or directory under the directory named `__tests__/`
within the project root. However, unlike the previous example's
`.*/__tests__/.*`, it would NOT ignore files or directories under other
directories named `__tests__/`, like `src/__tests__/`.

### Exclusions <a class="toc" id="toc-ignore-exclusions" href="#toc-ignore-exclusions"></a>
Sometimes you may want to ignore all files inside a directory with the exception of a few. An optional prefix "!" which negates the pattern may help. With this, any matching file excluded by a previous pattern will become included again.

```
[ignore]
<PROJECT_ROOT>/node_modules/.*
!<PROJECT_ROOT>/node_modules/not-ignored-package-A/.*
!<PROJECT_ROOT>/node_modules/not-ignored-package-B/.*
```
