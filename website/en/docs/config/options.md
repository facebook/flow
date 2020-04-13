---
layout: guide
---

The `[options]` section in a `.flowconfig` file can contain several key-value
pairs of the form:

```
[options]
keyA=valueA
keyB=valueB
```

Any options that are omitted will use their default values. Some options
can be overridden with command line flags.

### Available options <a class="toc" id="toc-available-options" href="#toc-available-options"></a>

* [`all`](#toc-all-boolean)
* [`babel_loose_array_spread`](#toc-babel-loose-array-spread-boolean)
* [`emoji`](#toc-emoji-boolean)
* [`esproposal.class_instance_fields`](#toc-esproposal-class-instance-fields-enable-ignore-warn)
* [`esproposal.class_static_fields`](#toc-esproposal-class-static-fields-enable-ignore-warn)
* [`esproposal.decorators`](#toc-esproposal-decorators-ignore-warn)
* [`esproposal.export_star_as`](#toc-esproposal-export-star-as-enable-ignore-warn)
* [`esproposal.optional_chaining`](#toc-esproposal-optional-chaining-enable-ignore-warn)
* [`esproposal.nullish_coalescing`](#toc-esproposal-nullish-coalescing-enable-ignore-warn)
* [`exact_by_default`](#toc-exact-by-default-boolean)
* [`experimental.const_params`](#toc-experimental-const-params-boolean)
* [`include_warnings`](#toc-include-warnings-boolean)
* [`lazy_mode`](#toc-lazy-mode-fs-ide-watchman-none)
* [`log.file`](#toc-log-file-string)
* [`max_header_tokens`](#toc-max-header-tokens-integer)
* [`module.file_ext`](#toc-module-file-ext-string)
* [`module.ignore_non_literal_requires`](#toc-module-ignore-non-literal-requires-boolean)
* [`module.name_mapper`](#toc-module-name-mapper-regex-string)
* [`module.name_mapper.extension`](#toc-module-name-mapper-extension-string-string)
* [`module.system`](#toc-module-system-node-haste)
* [`module.system.node.main_field`](#toc-module-system-node-main-field-string)
* [`module.system.node.resolve_dirname`](#toc-module-system-node-resolve-dirname-string)
* [`module.use_strict`](#toc-module-use-strict-boolean)
* [`munge_underscores`](#toc-munge-underscores-boolean)
* [`no_flowlib`](#toc-no-flowlib-boolean)
* [`react.runtime`](#toc-react-runtime-automatic-classic)
* [`server.max_workers`](#toc-server-max-workers-integer)
* [`sharedmemory.dirs`](#toc-sharedmemory-dirs-string)
* [`sharedmemory.minimum_available`](#toc-sharedmemory-minimum-available-unsigned-integer)
* [`sharedmemory.hash_table_pow`](#toc-sharedmemory-hash-table-pow-unsigned-integer)
* [`sharedmemory.heap_size`](#toc-sharedmemory-heap-size-unsigned-integer)
* [`sharedmemory.log_level`](#toc-sharedmemory-log-level-unsigned-integer)
* [`strip_root`](#toc-strip-root-boolean)
* [`suppress_comment`](#toc-suppress-comment-regex)
* [`suppress_type`](#toc-suppress-type-string)
* [`temp_dir`](#toc-temp-dir-string)
* [`traces`](#toc-traces-integer)

#### `all` _`(boolean)`_ <a class="toc" id="toc-all-boolean" href="#toc-all-boolean"></a>

Set this to `true` to check all files, not just those with `@flow`.

The default value for `all` is `false`.

#### `babel_loose_array_spread` _`(boolean)`_ <a class="toc" id="toc-babel-loose-array-spread-boolean" href="#toc-babel-loose-array-spread-boolean"></a>

Set this to `true` to check that array spread syntax is only used with arrays, not arbitrary iterables (such as `Map` or `Set`). This is useful if you transform your code with Babel in [loose mode](https://babeljs.io/docs/en/babel-plugin-transform-spread#loose) which makes this non-spec-compliant assumption at runtime.

For example:

```js
const set = new Set();
const values = [...set]; // Valid ES2015, but Set is not compatible with $ReadOnlyArray in Babel loose mode
```

The default value for `babel_loose_array_spread` is `false`.

#### `emoji` _`(boolean)`_ <a class="toc" id="toc-emoji-boolean" href="#toc-emoji-boolean"></a>

Set this to `true` to add emoji to the status messages that Flow
outputs when it's busy checking your project.

The default value for `emoji` is `false`.

#### `esproposal.class_instance_fields` _`(enable|ignore|warn)`_ <a class="toc" id="toc-esproposal-class-instance-fields-enable-ignore-warn" href="#toc-esproposal-class-instance-fields-enable-ignore-warn"></a>

Set this to `warn` to indicate that Flow should give a warning on use of
instance [class fields](https://github.com/tc39/proposal-class-public-fields)
per the pending spec.

You may also set this to `ignore` to indicate that Flow should simply ignore
the syntax (i.e. Flow will not use this syntax to indicate the presence of a
property on instances of the class).

The default value of this option is `enable`, which allows use of this proposed
syntax.

#### `esproposal.class_static_fields` _`(enable|ignore|warn)`_ <a class="toc" id="toc-esproposal-class-static-fields-enable-ignore-warn" href="#toc-esproposal-class-static-fields-enable-ignore-warn"></a>

Set this to `warn` to indicate that Flow should give a warning on use of static
[class fields](https://github.com/tc39/proposal-class-public-fields)
per the pending spec.

You may also set this to `ignore` to indicate that Flow should simply ignore
the syntax (i.e. Flow will not use this syntax to indicate the presence of a
static property on the class).

The default value of this option is `enable`, which allows use of this proposed
syntax.

#### `esproposal.decorators` _`(ignore|warn)`_ <a class="toc" id="toc-esproposal-decorators-ignore-warn" href="#toc-esproposal-decorators-ignore-warn"></a>

Set this to `ignore` to indicate that Flow should ignore decorators.

The default value of this option is `warn`, which gives a warning on use since
this proposal is still very early-stage.

#### `esproposal.export_star_as` _`(enable|ignore|warn)`_ <a class="toc" id="toc-esproposal-export-star-as-enable-ignore-warn" href="#toc-esproposal-export-star-as-enable-ignore-warn"></a>

Set this to `enable` to indicate that Flow should support the `export * as`
syntax from [leebyron's proposal](https://github.com/leebyron/ecmascript-more-export-from).

You may also set this to `ignore` to indicate that Flow should simply ignore
the syntax. The default value of this option is `warn`, which gives a warning
on use since this proposal is still very early-stage.

#### `esproposal.optional_chaining` _`(enable|ignore|warn)`_ <a class="toc" id="toc-esproposal-optional-chaining-enable-ignore-warn" href="#toc-esproposal-optional-chaining-enable-ignore-warn"></a>

Set this to `enable` to indicate that Flow should support the use of
[optional chaining](https://github.com/tc39/proposal-optional-chaining)
per the pending spec.

You may also set this to `ignore` to indicate that Flow should simply ignore
the syntax.

The default value of this option is `warn`, which gives a warning on
use since this proposal is still very early-stage.

#### `esproposal.nullish_coalescing` _`(enable|ignore|warn)`_ <a class="toc" id="toc-esproposal-nullish-coalescing-enable-ignore-warn" href="#toc-esproposal-nullish-coalescing-enable-ignore-warn"></a>

Set this to `enable` to indicate that Flow should support the use of
[nullish coalescing](https://github.com/tc39/proposal-nullish-coalescing)
per the pending spec.

You may also set this to `ignore` to indicate that Flow should simply ignore
the syntax.

The default value of this option is `warn`, which gives a warning on
use since this proposal is still very early-stage.

#### `exact_by_default` _`(boolean)`_ <a class="toc" id="toc-exact-by-default-boolean" href="#toc-exact-by-default-boolean"></a>

Set this to `true` to indicate that Flow should interpret object types as exact
by default. When this flag is `false`, Flow has the following behavior:

```js
{foo: number} // inexact
{| foo: number |} // exact
{foo: number, ...} // inexact
```

When this flag is `true`, Flow has the following behavior:

```js
{foo: number} // exact
{| foo: number |} // exact
{foo: number, ...} // inexact
```

The default value is `false`.

#### `experimental.const_params` _`(boolean)`_ <a class="toc" id="toc-experimental-const-params-boolean" href="#toc-experimental-const-params-boolean"></a>

Setting this to `true` makes Flow treat all function parameters as const
bindings. Reassigning a param is an error which lets Flow be less conservative
with refinements.

The default value is `false`.

#### `include_warnings` _`(boolean)`_ <a class="toc" id="toc-include-warnings-boolean" href="#toc-include-warnings-boolean"></a>

Setting this to `true` makes Flow commands include warnings in the error output.
Warnings are hidden by default in the CLI to avoid console spew. (An IDE is a
much better interface to show warnings.)

The default value is `false`.

#### `lazy_mode` _`(fs|ide|watchman|none)`_ <a class="toc" id="toc-lazy-mode-fs-ide-watchman-none" href="#toc-lazy-mode-fs-ide-watchman-none"></a>

For more on lazy modes, see the [lazy modes docs](/en/docs/lang/lazy-modes/).

Setting `lazy_mode` in the `.flowconfig` will cause new Flow servers for that
root to use that lazy mode (or no lazy mode if set to `none`). This option can
be overridden from the CLI using the `--lazy-mode` flag.

The default value is `none`.

#### `log.file` _`(string)`_ <a class="toc" id="toc-log-file-string" href="#toc-log-file-string"></a>

The path to the log file (defaults to `/tmp/flow/<escaped root path>.log`).

#### `max_header_tokens` _`(integer)`_ <a class="toc" id="toc-max-header-tokens-integer" href="#toc-max-header-tokens-integer"></a>

Flow tries to avoid parsing non-flow files. This means Flow needs to
start lexing a file to see if it has `@flow` or `@noflow` in it. This option
lets you configure how much of the file Flow lexes before it decides there is
no relevant docblock.

- Neither `@flow` nor `@noflow` - Parse this file with Flow syntax disallowed and do not typecheck it.
- @flow - Parse this file with Flow syntax allowed and typecheck it.
- @noflow - Parse this file with Flow syntax allowed and do not typecheck it. This is meant as an escape hatch to suppress Flow in a file without having to delete all the Flow-specific syntax.

The default value of `max_header_tokens` is 10.

#### `module.file_ext` _`(string)`_ <a class="toc" id="toc-module-file-ext-string" href="#toc-module-file-ext-string"></a>

By default, Flow will look for files with the extensions `.js`, `.jsx`, `.mjs`
and `.json`. You can override this behavior with this option.

For example, if you do:

```
[options]
module.file_ext=.foo
module.file_ext=.bar
```

Then Flow will instead look for the file extensions `.foo` and `.bar`.

> **Note:** you can specify `module.file_ext` multiple times

#### `module.ignore_non_literal_requires` _`(boolean)`_ <a class="toc" id="toc-module-ignore-non-literal-requires-boolean" href="#toc-module-ignore-non-literal-requires-boolean"></a>

Set this to `true` and Flow will no longer complain when you use `require()`
with something other than a string literal.

The default value is `false`.

#### `module.name_mapper` _`(regex -> string)`_ <a class="toc" id="toc-module-name-mapper-regex-string" href="#toc-module-name-mapper-regex-string"></a>

Specify a regular expression to match against module names, and a replacement
pattern, separated by a `->`.

For example:

```
module.name_mapper='^image![a-zA-Z0-9$_]+$' -> 'ImageStub'
```

This makes Flow treat `require('image!foo.jpg')` as if it were
`require('ImageStub')`.

These are [OCaml regular expressions](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html#TYPEregexp).
Use `\(` and `\)` (slashes required!) to create a capturing group, which you
can refer to in the replacement pattern as `\1` (up to `\9`).

> **Note:** you can specify `module.name_mapper` multiple times

#### `module.name_mapper.extension` _`(string -> string)`_ <a class="toc" id="toc-module-name-mapper-extension-string-string" href="#toc-module-name-mapper-extension-string-string"></a>

Specify a file extension to match, and a replacement module name, separated by
a `->`.

> **Note:** This is just shorthand for
> `module.name_mapper='^\(.*\)\.EXTENSION$' -> 'TEMPLATE'`)

For example:

```
module.name_mapper.extension='css' -> '<PROJECT_ROOT>/CSSFlowStub.js.flow'
```

Makes Flow treat `require('foo.css')` as if it were
`require(PROJECT_ROOT + '/CSSFlowStub')`.

> **Note:** You can specify `module.name_mapper.extension` multiple times for
> different extensions.

#### `module.system` _`(node|haste)`_ <a class="toc" id="toc-module-system-node-haste" href="#toc-module-system-node-haste"></a>

The module system to use to resolve `import` and `require`.
[Haste](https://github.com/facebook/node-haste) is used in React Native.

The default is `node`.

#### `module.system.node.main_field` _`(string)`_ <a class="toc" id="toc-module-system-node-main-field-string" href="#toc-module-system-node-main-field-string"></a>

Flow reads `package.json` files for the `"name"` and `"main"` fields to figure
out the name of the module and which file should be used to provide that
module.

So if Flow sees this in the `.flowconfig`:

```
[options]
module.system.node.main_field=foo
module.system.node.main_field=bar
module.system.node.main_field=baz
```

and then it comes across a `package.json` with

```
{
  "name": "kittens",
  "main": "main.js",
  "bar": "bar.js",
  "baz": "baz.js"
}
```

Flow will use `bar.js` to provide the `"kittens"` module.

If this option is unspecified, Flow will always use the `"main"` field.

See [this GitHub issue for the original motivation](https://github.com/facebook/flow/issues/5725)

#### `module.system.node.resolve_dirname` _`(string)`_ <a class="toc" id="toc-module-system-node-resolve-dirname-string" href="#toc-module-system-node-resolve-dirname-string"></a>

By default, Flow will look in directories named `node_modules` for node
modules. You can configure this behavior with this option.

For example, if you do:

```
[options]
module.system.node.resolve_dirname=node_modules
module.system.node.resolve_dirname=custom_node_modules
```

Then Flow will look in directories named `node_modules` or
`custom_node_modules`.

> **Note:** you can specify `module.system.node.resolve_dirname` multiple times

#### `module.use_strict` _`(boolean)`_ <a class="toc" id="toc-module-use-strict-boolean" href="#toc-module-use-strict-boolean"></a>

Set this to `true` if you use a transpiler that adds `"use strict";` to the top
of every module.

The default value is `false`.

#### `munge_underscores` _`(boolean)`_ <a class="toc" id="toc-munge-underscores-boolean" href="#toc-munge-underscores-boolean"></a>

Set this to `true` to have Flow treat underscore-prefixed class properties and
methods as private. This should be used in conjunction with [`jstransform`'s
ES6 class transform](https://github.com/facebook/jstransform/blob/master/visitors/es6-class-visitors.js),
which enforces the same privacy at runtime.

The default value is `false`.

#### `no_flowlib` _`(boolean)`_ <a class="toc" id="toc-no-flowlib-boolean" href="#toc-no-flowlib-boolean"></a>

Flow has builtin library definitions. Setting this to `true` will tell Flow to
ignore the builtin library definitions.

The default value is `false`.

#### `react.runtime` _`(automatic|classic)`_ <a class="toc" id="toc-react-runtime-automatic-classic" href="#toc-react-runtime-automatic-classic"></a> {% since 0.123.0 %}

Set this to `automatic` if you are using React's automatic runtime in `@babel/plugin-transform-react-jsx`.
Otherwise, use `classic`. [See the babel documentation](https://babeljs.io/docs/en/babel-plugin-transform-react-jsx)
for details about the transform.

The default value is `automatic`.

#### `server.max_workers` _`(integer)`_ <a class="toc" id="toc-server-max-workers-integer" href="#toc-server-max-workers-integer"></a>

The maximum number of workers the Flow server can start. By default, the server
will use all available cores.

#### `sharedmemory.dirs` _`(string)`_ <a class="toc" id="toc-sharedmemory-dirs-string" href="#toc-sharedmemory-dirs-string"></a>

This affects Linux only.

Flow's shared memory lives in a memory mapped file. On more modern versions of
Linux (3.17+), there is a system call `memfd_create` which allows Flow to create
the file anonymously and only in memory. However, in older kernels, Flow needs
to create a file on the file system. Ideally this file lives on a memory-backed
tmpfs. This option lets you decide where that file is created.

By default this option is set to `/dev/shm` and `/tmp`

> **Note:** You can specify `sharedmemory.dirs` multiple times.

#### `sharedmemory.minimum_available` _`(unsigned integer)`_ <a class="toc" id="toc-sharedmemory-minimum-available-unsigned-integer" href="#toc-sharedmemory-minimum-available-unsigned-integer"></a>


This affects Linux only.

As explained in the [`sharedmemory.dirs`]() option's description, Flow needs to
create a file on a filesystem for older kernels. `sharedmemory.dirs` specifies
a list of locations where the shared memory file can be created. For each
location, Flow will check to make sure the filesystem has enough space for the
shared memory file. If Flow will likely run out of space, it skips that location
and tries the next. This option lets you configure the minimum amount of space
needed on a filesystem for shared memory.

By default it is 536870912 (2^29 bytes, which is half a gigabyte).

#### `sharedmemory.hash_table_pow` _`(unsigned integer)`_ <a class="toc" id="toc-sharedmemory-hash-table-pow-unsigned-integer" href="#toc-sharedmemory-hash-table-pow-unsigned-integer"></a>

The 3 largest parts of the shared memory are a dependency table, a hash table,
and a heap. While the heap grows and shrinks, the two tables are allocated in
full. This option lets you change the size of the hash table.

Setting this option to X means the table will support up to 2^X elements,
which is 16*2^X bytes.

By default, this is set to 19 (Table size is 2^19, which is 8 megabytes)

#### `sharedmemory.heap_size` _`(unsigned integer)`_ <a class="toc" id="toc-sharedmemory-heap-size-unsigned-integer" href="#toc-sharedmemory-heap-size-unsigned-integer"></a>

This option configures the maximum possible size for the shared heap. You should
most likely not need to configure this, as it doesn't really affect how much
RSS Flow uses. However, if you are working on a massive codebase you might see
the following error after init: "Heap init size is too close to max heap size;
GC will never get triggered!" In this case, you may need to increase the size
of the heap.

By default, this is set to 26843545600 (25 * 2^30 bytes, which is 25GiB)

#### `sharedmemory.log_level` _`(unsigned integer)`_ <a class="toc" id="toc-sharedmemory-log-level-unsigned-integer" href="#toc-sharedmemory-log-level-unsigned-integer"></a>

Setting this to 1 will cause Flow to output some stats about the data that is
serialized into and deserialized out of shared memory.

By default this is 0.

#### `strip_root` _`(boolean)`_ {% until 0.48 %} <a class="toc" id="toc-strip-root-boolean" href="#toc-strip-root-boolean"></a>

Obsolete. Set this to `true` to always strip the root directory from file paths
in error messages when using `--json`, `--from emacs`, and `--from vim`.
Do not use this option. Instead, pass the command line flag `--strip-root`.

By default this is `false`.

#### `suppress_comment` _`(regex)`_ <a class="toc" id="toc-suppress-comment-regex" href="#toc-suppress-comment-regex"></a>

Defines a magical comment that suppresses any Flow errors on the following
line. For example:

```
suppress_comment= \\(.\\|\n\\)*\\$FlowFixMe
```

will match a comment like this:

```
// $FlowFixMe: suppressing this error until we can refactor
var x : string = 123;
```

and suppress the error. If there is no error on the next line (the suppression
is unnecessary), an "Unused suppression" warning will be shown instead.

If no suppression comments are specified in your config, Flow will apply one
default: `// $FlowFixMe`.

> **Note:** You can specify `suppress_comment` multiple times. If you do define
> any `suppress_comment`s, the built-in `$FlowFixMe` suppression will be erased
> in favor of the regexps you specify. If you wish to use `$FlowFixMe` with
> some additional custom suppression comments, you must manually specify
> `\\(.\\|\n\\)*\\$FlowFixMe` in your custom list of suppressions.

#### `suppress_type` _`(string)`_ <a class="toc" id="toc-suppress-type-string" href="#toc-suppress-type-string"></a>

This option lets you alias `any` with a given string. This is useful for
explaining why you're using `any`. For example, let's say you sometimes want
to sometimes use `any` to suppress an error and sometimes to mark a TODO.
Your code might look like

```
var myString: any = 1 + 1;
var myBoolean: any = 1 + 1;
```

If you add the following to your configuration:

```
[options]
suppress_type=$FlowFixMe
suppress_type=$FlowTODO
```

You can update your code to the more readable:

```
var myString: $FlowFixMe = 1 + 1;
var myBoolean: $FlowTODO = 1 + 1;
```

> **Note:** You can specify `suppress_type` multiple times.

#### `temp_dir` _`(string)`_ <a class="toc" id="toc-temp-dir-string" href="#toc-temp-dir-string"></a>

Tell Flow which directory to use as a temp directory. Can be overridden with the
command line flag `--temp-dir`.

The default value is `/tmp/flow`.

#### `traces` _`(integer)`_ <a class="toc" id="toc-traces-integer" href="#toc-traces-integer"></a>

Enables traces on all error output (showing additional details about the flow
of types through the system), to the depth specified. This can be very
expensive, so is disabled by default.
