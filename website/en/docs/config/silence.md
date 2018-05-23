---
layout: guide
---

Often third-party libraries have broken type definitions or have type
definitions only compatible with a certain version of Flow. In those cases it
may be useful to still type check the third-party libraries but silence any
warnings or errors found.

### `[silence]` <a class="toc" id="toc-silence" href="#toc-silence"></a>

The `[silence]` section in a `.flowconfig` file tells Flow to silence warnings
and errors in files matching the specified regular expressions when type
checking your code. Flow still type checks the files but acts as if there is
a comment that matches
[`suppress_comment`](options#toc-suppress-comment-regex") on every line.
By default, nothing is silenced.

Things to keep in mind:

1.  These are [OCaml regular expressions](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html#TYPEregexp).
2.  These regular expressions match against absolute paths. They probably should
    start with `.*`

An example `[silence]` section might look like:

```
[silence]
.*/third_party/.*
.*/src/\(foo\|bar\)/.*
.*\.silence\.js
```

This `[silence]` section will ignore:

1.  Any file or directory under a directory named `third_party`
2.  Any file or directory under `.*/src/foo` or under `.*/src/bar`
3.  Any file that ends with the extension `.silence.js`

Starting with Flow v0.23.0, you may use the `<PROJECT_ROOT>` placeholder in
your regular expressions. At runtime, Flow will treat the placeholder as if it
were the absolute path to the project's root directory. This is useful for
writing regular expressions that are relative rather than absolute.

For example, you can write:

```
[silence]
<PROJECT_ROOT>/third_party/.*
```

Which would silence any file or directory under the directory named `third_party/`
within the project root. However, unlike the previous example's
`.*/third_party/.*`, it would NOT silence files or directories under other
directories named `third_party/`, like `src/third_party/`.
