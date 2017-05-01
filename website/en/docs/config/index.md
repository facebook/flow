---
layout: guide
---

Every Flow project contains a `.flowconfig` file. You can configure Flow by
modifying `.flowconfig`. New projects or projects that are starting to use Flow
can generate a default `.flowconfig` by running `flow init`.

### `.flowconfig` format <a class="toc" id="toc-flowconfig-format" href="#toc-flowconfig-format"></a>

The `.flowconfig` uses a custom format that vaguely resembles INI files. We are
not proud of our custom format and plan to support a better format in the
future. [GitHub issue #153](https://github.com/facebook/flow/issues/153) tracks
this.

The `.flowconfig` consists of 5 sections:

* [`[include]`](include)
* [`[ignore]`](ignore)
* [`[libs]`](libs)
* [`[options]`](options)
* [`[version]`](version)

### Comments <a class="toc" id="toc-comments" href="#toc-comments"></a>

Comment support was added in v0.23.0. Lines beginning with zero or more spaces
followed by an `#` or `;` or `💩` are ignored.  For example:

```
# This is a comment
  # This is a comment
; This is a comment
  ; This is a comment
💩 This is a comment
  💩 This is a comment
```

### Where to put the `.flowconfig` <a class="toc" id="toc-where-to-put-the-flowconfig" href="#toc-where-to-put-the-flowconfig"></a>

The location of the `.flowconfig` is significant. Flow treats the directory that
contains the `.flowconfig` as the *project root*. By default Flow includes all
the source code under the project root. The paths in the
[[include] section](include) are relative to the project root. Some other
configuration also lets you reference the project root via the macro
``<PROJECT_ROOT>``.

Most people put the `.flowconfig` in the root of their project (i.e. next to the
`package.json`). Some people put all their code in a `src/` directory and
therefore put the `.flowconfig` at `src/.flowconfig`.


### Example <a class="toc" id="toc-example" href="#toc-example"></a>

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
check, ignore the `build` directory and use the declarations in  `lib`.
