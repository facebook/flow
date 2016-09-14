---
id: advanced-configuration
title: Advanced Configuration
permalink: /docs/advanced-configuration.html
prev: cli.html
next: quick-reference.html
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
specified files or directories when type checking your code. Including a
directory includes all the files under that directory. Each line in the
include section is a path to include. These paths can be relative to the root
directory or absolute, and support both single and double star wildcards.

The root directory (where your `.flowconfig` lives) is automatically included.

For example, if `/path/to/root/.flowconfig` contains the following `[include]`
section:

```
[include]
../externalFile.js
../externalDir/
../otherProject/*.js
../otherProject/**/coolStuff/
```

then when Flow checks the project in `/path/to/root`, it will read and watch

1. `/path/to/root/` (automatically included)
2. `/path/to/externalFile.js`
3. `/path/to/externalDir/`
4. Any file in `/path/to/otherProject/` that ends in `.js`
5. Any directory under `/path/to/otherProject` named `coolStuff/`

### `[ignore]`

The `[ignore]` heading in a `.flowconfig` file tells `flow` to ignore files
matching the specified regular expressions when type checking your code. By
default, nothing is ignored.

Things to keep in mind:

1. These are [OCaml regular expressions](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html#TYPEregexp).
2. These regular expressions match against absolute paths. They probably should
   start with `.*`
3. Ignores are processed AFTER includes. If you include and ignore a file it
   will be ignored.

An example `[ignore]` section might look like

```
[ignore]
.*/__tests__/.*
.*/src/\(foo\|bar\)/.*
.*\.ignore\.js
```

This `[ignore]` section will ignore

1. Any file or directory under a directory named `__tests__`
2. Any file or directory under `.*/src/foo` or under `.*/src/bar`
3. Any file that ends with the extension `.ignore.js`

Starting with Flow v0.23.0, you may use the `<PROJECT_ROOT>` placeholder in
your regular expressions. At runtime, Flow will treat the placeholder as if it
were the absolute path to the project's root directory. This is useful for
writing regular expressions that are relative rather than absolute. For
example, you can write

```
[ignore]
<PROJECT_ROOT>/__tests__/.*
```

which would ignore any file or directory under the directory named `__tests__/`
within the project root. However, unlike the previous example's
`.*/__tests__/.*`, it would NOT ignore files or directories under other
directories named `__tests__/`, like `src/__tests__/`.

### `[libs]`

The `[libs]` heading in a `.flowconfig` file tells `flow` to include the
specified [declarations](declarations.html) when type
checking your code. Multiple libraries can be specified. By default, no
additional libraries are included.

Each line in the `[libs]` section is a path to the lib file or lib directory
which you would like to include. These paths can be relative to the root
directory or absolute.

### `[options]`

The `[options]` heading in a `.flowconfig` file can contain several key-value
pairs. Any options that are omitted will use their default values. Some options
can be overridden with command line flags.

- `log.file` (string): the path to the log file (defaults to `/tmp/flow/<escaped root path>.log`)
- `module.name_mapper` (regex -> string): specify a regular expression to match against module names, and a replacement pattern, separated by a `->`.

  For example:

  ```
  module.name_mapper='^image![a-zA-Z0-9$_]+$' -> 'ImageStub'
  ```

  makes Flow treat `require('image!foo.jpg')` as if it were `require('ImageStub')`.

  These are [OCaml regular expressions](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html#TYPEregexp). Use `\(` and `\)` (slashes required!) to create a capturing group, which you can refer to in the replacement pattern as `\1` (up to `\9`).

  (**note:** you can specify `module.name_mapper` multiple times)

- `module.name_mapper.extension` (string -> string): specify a file extension to match, and a replacement module name, separated by a `->`.

  *(Note that this is just shorthand for `module.name_mapper='^\(.*\)\.EXTENSION$' -> 'TEMPLATE'`)*

  For example:

  ```
  module.name_mapper.extension='css' -> '<PROJECT_ROOT>/CSSFlowStub.js.flow'
  ```

  makes Flow treat `require('foo.css')` as if it were `require(PROJECT_ROOT + '/CSSFlowStub')`.

  (**note:** you can specify `module.name_mapper.extension` multiple times for different extensions)

- `module.system` (`node` | `haste`): the module system to use to resolve
  `import` and `require`. [Haste](https://github.com/facebook/node-haste) is
  used in React Native. The default is `node`.

- `module.system.node.resolve_dirname` (string): By default, Flow will look in
  directories named `node_modules` for node modules. You can configure this
  behavior with this option. For example, if you do

  ```
  [options]
  module.system.node.resolve_dirname=node_modules
  module.system.node.resolve_dirname=custom_node_modules
  ```

  then Flow will look in directories named `node_modules` or
  `custom_node_modules`

- `module.ignore_non_literal_requires` (boolean): set this to `true` and Flow
  will no longer complain when you use `require()` with something other than a
  string literal. The default value is `false`

- `module.file_ext` (string): By default, Flow will look for files with the extensions
  `.js`, `.jsx`, and `.json`. You can override this behavior with this option.
  For example, if you do

  ```
  [options]
  module.file_ext=.foo
  module.file_ext=.bar
  ```

  then Flow will instead look for the file extensions `.foo` and `.bar`.

- `module.use_strict` (boolean): set this to `true` if you use a transpiler that
  adds `"use strict";` to the top of every module. The default value is `false`.

- `munge_underscores` (boolean): Set this to `true` to have Flow treat
  underscore-prefixed class properties and methods as private. This should be
  used in conjunction with [`jstransform`'s ES6 class
  transform](https://github.com/facebook/jstransform/blob/master/visitors/es6-class-visitors.js),
  which enforces the same privacy at runtime.

- `server.max_workers` (integer): The maximum number of workers the Flow server
  can start. By default, the server will use all available cores.

- `traces` (integer): enables traces on all error output (showing additional
  details about the flow of types through the system), to the depth specified.
  This can be very expensive, so is disabled by default.

- `strip_root` (boolean): set this to `true` to always strip the root directory
  from file paths in error messages. Can be overridden with the command line
  flag `--strip-root`.

- `suppress_comment` (regex): defines a magical comment that suppresses any Flow errors on the following line. For example:

  ```
  suppress_comment= \\(.\\|\n\\)*\\$FlowFixMe
  ```

  will match a comment like this:

  ```
  // $FlowFixMe: suppressing this error until we can refactor
  var x : string = 123;
  ```

  and suppress the error. If there is no error on the next line (the suppression is unnecessary), an "Unused suppression" error will be shown instead.

  If no suppression comments are specified in your config, Flow will apply one default: `// $FlowFixMe`.

  **Note:** You can specify `suppress_comment` multiple times. If you do define any `suppress_comment`s, the built-in `$FlowFixMe` suppression will be erased in favor of the regexps you specify. If you wish to use `$FlowFixMe` with some additional custom suppression comments, you must manually specify `\\(.\\|\n\\)*\\$FlowFixMe` in your custom list of suppressions.

- `temp_dir` (string): Tell Flow which directory to use as a temp directory.
  Defaults to `/tmp/flow`. Can be overridden with the commandline flag
  `--temp-dir`.

- `esproposal.class_static_fields` (`enable`|`ignore`|`warn`): set this to
  `warn` to indicate that Flow should give a warning on use of static [class
  fields](https://github.com/jeffmo/es-class-fields-and-static-properties) per
  the pending spec. You may also set this to `ignore` to indicate that Flow
  should simply ignore the syntax (i.e. Flow will not use this syntax to
  indicate the presence of a static property on the class). The default value
  of this option is `enable`, which allows use of this proposed syntax.

- `esproposal.class_instance_fields` (`enable`|`ignore`|`warn`): set this to
  `warn` to indicate that Flow should give a warning on use of instance [class
  fields](https://github.com/jeffmo/es-class-fields-and-static-properties) per
  the pending spec. You may also set this to `ignore` to indicate that Flow
  should simply ignore the syntax (i.e. Flow will not use this syntax to
  indicate the presence of a property on instances of the class). The default
  value of this option is `enable`, which allows use of this proposed syntax.

- `esproposal.decorators` (`ignore`|`warn`): set this to
  `ignore` to indicate that Flow should ignore decorators. The default
  value of this option is `warn`, which gives a warning on use since this
  proposal is still very early-stage.

- `esproposal.export_star_as` (`enable`|`ignore`|`warn`): set this to
  `enable` to indicate that Flow should support the `export * as` syntax from
  [leebyron's proposal](https://github.com/leebyron/ecmascript-more-export-from).
  You may also set this to `ignore` to indicate that Flow
  should simply ignore the syntax. The default value of this option is `warn`,
  which gives a warning on use since this proposal is still very early-stage.

- `all` (boolean) - set this to check all files, not just those with `@flow`

### [version]

You can specify in the `.flowconfig` which version of Flow you expect to use.
You do this with the `[version]` section. If this section is omitted or left
blank, then any version is allowed. If a version is specified and not matched,
then Flow will immediately error and exit.

So if you have the following in your .flowconfig

```
[version]
0.22.0
```

and you try to use Flow v0.21.0, then Flow will immediately error with the
message

`"Wrong version of Flow. The config specifies version 0.22.0 but this is version
0.21.0"`

So far, we support the following ways to specify supported versions

- Explicit versions, (e.g. `0.22.0`, which only matches `0.22.0`).
- Intersection ranges, which are ANDed together, (e.g. `>=0.13.0 <0.14.0`,
  which matches `0.13.0` and `0.13.5` but not `0.14.0`).
- Caret ranges, which allow changes that do not modify the left-most non-zero
  digit (e.g.  `^0.13.0` expands into `>=0.13.0 <0.14.0`, and `^0.13.1` expands
  into `>=0.13.1 <0.14.0`, whereas "^1.2.3" expands into ">=1.2.3 <2.0.0").

### Comments

Comment support was added in v0.23.0. Lines beginning with 0 or more spaces
followed by an `#` or `;` are ignored.  For example:

```
# This is a comment
   # This is a comment
; This is a comment
   ; This is a comment
```

### Example

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
