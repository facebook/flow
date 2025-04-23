---
title: .flowconfig [options]
slug: /config/options
---

import {SinceVersion, UntilVersion} from '../../components/VersionTags';

The `[options]` section in a `.flowconfig` file can contain several key-value
pairs of the form:

```
[options]
keyA=valueA
keyB=valueB
```

Any options that are omitted will use their default values. Some options
can be overridden with command line flags.

## Available options {#toc-available-options}

### all {#toc-all}

Type: `boolean`

Set this to `true` to check all files, not just those with `@flow`.

The default value for `all` is `false`.

### autoimports <SinceVersion version="0.143.0" /> {#toc-autoimports}

Type: `boolean`

When enabled, IDE autocomplete suggests the exports of other files, and the necessary `import` statements are automatically inserted. A "quick fix" code action is also provided on undefined variables that suggests matching imports.

The default value for `autoimports` is `true` as of Flow v0.155.0.

### babel_loose_array_spread {#toc-babel-loose-array-spread}

Type: `boolean`

Set this to `true` to check that array spread syntax is only used with arrays, not arbitrary iterables (such as `Map` or `Set`). This is useful if you transform your code with Babel in [loose mode](https://babeljs.io/docs/en/babel-plugin-transform-spread#loose) which makes this non-spec-compliant assumption at runtime.

For example:

```js
const set = new Set();
const values = [...set]; // Valid ES2015, but Set is not compatible with $ReadOnlyArray in Babel loose mode
```

The default value for `babel_loose_array_spread` is `false`.

### emoji {#toc-emoji}

Type: `boolean`

Set this to `true` to add emoji to the status messages that Flow
outputs when it's busy checking your project.

The default value for `emoji` is `false`.

### enums {#toc-enums}

Type: `boolean`

Set this to `true` to enable [Flow Enums](../../enums).
[Additional steps](../../enums/enabling-enums/) are required beyond just enabling the `.flowconfig` option.

The default value for `enums` is `false`.

### exact_by_default {#toc-exact-by-default}

Type: `boolean`

When set to `true` (the default as of version 0.202), Flow interprets object types as exact by default:

```js flow-check
type O1 = {foo: number} // exact
type O2 = {| foo: number |} // exact
type O3 = {foo: number, ...} // inexact
```

When this flag is `false`, Flow has the following behavior:

```js flow-check
type O1 = {foo: number} // inexact
type O2 = {| foo: number |} // exact
type O3 = {foo: number, ...} // inexact
```

- From inception to Flow version 0.199, the default value of the flag was `false`.
- In versions 0.200 and 0.201, the flag was required to be explicitly set to either `true` or `false`.
- From version 0.202, the default value is `true`.

You can read more about this change in our blog post about making [exact by object types by default, by default](https://medium.com/flow-type/exact-object-types-by-default-by-default-cc559af6f69).

### experimental.const_params {#toc-experimental-const-params}

Type: `boolean`

Setting this to `true` makes Flow treat all function parameters as const
bindings. Reassigning a param is an error which lets Flow be less conservative
with refinements.

The default value is `false`.

### include_warnings {#toc-include-warnings}

Type: `boolean`

Setting this to `true` makes Flow commands include warnings in the error output.
Warnings are hidden by default in the CLI to avoid console spew. (An IDE is a
much better interface to show warnings.)

The default value is `false`.

### lazy_mode {#toc-lazy-mode}

Type: `boolean`

For more on lazy modes, see the [lazy modes docs](../../lang/lazy-modes/).

Setting `lazy_mode` in the `.flowconfig` will cause new Flow servers for that
root to use lazy mode (or no lazy mode if set to `false`). This option can
be overridden from the CLI using the `--lazy-mode` flag.

The default value is `false`.

### max_header_tokens {#toc-max-header-tokens}

Type: `integer`

Flow tries to avoid parsing non-flow files. This means Flow needs to
start lexing a file to see if it has `@flow` or `@noflow` in it. This option
lets you configure how much of the file Flow lexes before it decides there is
no relevant docblock.

- Neither `@flow` nor `@noflow` - Parse this file with Flow syntax disallowed and do not typecheck it.
- `@flow` - Parse this file with Flow syntax allowed and typecheck it.
- `@noflow` - Parse this file with Flow syntax allowed and do not typecheck it. This is meant as an escape hatch to suppress Flow in a file without having to delete all the Flow-specific syntax.

The default value of `max_header_tokens` is 10.

### module.file_ext {#toc-module-file-ext}

By default, Flow will look for files with the extensions `.js`, `.jsx`, `.mjs`,
`.cjs` and `.json`. You can override this behavior with this option.

For example, if you do:

```
[options]
module.file_ext=.foo
module.file_ext=.bar
```

Then Flow will instead look for the file extensions `.foo` and `.bar`.

> **Note:** you can specify `module.file_ext` multiple times

### module.ignore_non_literal_requires {#toc-module-ignore-non-literal-requires}

Type: `boolean`

Set this to `true` and Flow will no longer complain when you use `require()`
with something other than a string literal.

The default value is `false`.

### module.name_mapper {#toc-module-name-mapper}

Type: `regex -> string`

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

### module.name_mapper.extension {#toc-module-name-mapper-extension}

Type: `string -> string`

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

### module.system {#toc-module-system}

Type: `node | haste`

The module system to use to resolve `import` and `require`.
Haste mode is used by Meta.

The default is `node`.

### module.system.node.main_field {#toc-module-system-node-main-field}

Type: `string`

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

### module.system.node.resolve_dirname {#toc-module-system-node-resolve-dirname}

Type: `string`

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

### module.use_strict {#toc-module-use-strict}

Type: `boolean`

Set this to `true` if you use a transpiler that adds `"use strict";` to the top
of every module.

The default value is `false`.

### munge_underscores {#toc-munge-underscores}

Type: `boolean`

Set this to `true` to have Flow treat underscore-prefixed class properties and
methods as private. This should be used in conjunction with [`jstransform`'s
ES6 class transform](https://github.com/facebook/jstransform/blob/master/visitors/es6-class-visitors.js),
which enforces the same privacy at runtime.

The default value is `false`.

### no_flowlib {#toc-no-flowlib}

Type: `boolean`

Flow has builtin library definitions. Setting this to `true` will tell Flow to
ignore the builtin library definitions.

The default value is `false`.

### react.runtime <SinceVersion version="0.123.0" /> {#toc-react-runtime}

Type: `automatic | classic`

Set this to `automatic` if you are using React's automatic runtime in `@babel/plugin-transform-react-jsx`.
Otherwise, use `classic`. [See the babel documentation](https://babeljs.io/docs/en/babel-plugin-transform-react-jsx)
for details about the transform.

The default value is `classic`.

### server.max_workers {#toc-server-max-workers}

Type: `integer`

The maximum number of workers the Flow server can start. By default, the server
will use all available cores.

### sharedmemory.hash_table_pow {#toc-sharedmemory-hash-table-pow}

Type: `unsigned integer`

The 3 largest parts of the shared memory are a dependency table, a hash table,
and a heap. While the heap grows and shrinks, the two tables are allocated in
full. This option lets you change the size of the hash table.

Setting this option to X means the table will support up to 2^X elements,
which is 16*2^X bytes.

By default, this is set to 19 (Table size is 2^19, which is 8 megabytes)

### sharedmemory.heap_size {#toc-sharedmemory-heap-size}

Type: `unsigned integer`

This option configures the maximum possible size for the shared heap. You should
most likely not need to configure this, as it doesn't really affect how much
RSS Flow uses. However, if you are working on a massive codebase you might see
the following error after init: "Heap init size is too close to max heap size;
GC will never get triggered!" In this case, you may need to increase the size
of the heap.

By default, this is set to 26843545600 (25 * 2^30 bytes, which is 25GiB)

### relay_integration {#toc-relay-integration}

Type: `boolean`

This option enables Flow's [Relay](https://relay.dev) integration. With the
integration enabled Flow will infer the types of `graphql` tagged template
literals as being the types that the Relay compiler emitted for that
fragment/mutaiton/query/etc. This allows users to omit type parameters from
common Relay APIs like `useFragment` and `usePreloadedQuery`.

### relay_integration.excludes {#toc-relay-integration-excludes}

Type: `string`

This option allows you to exclude some directories in the project from using
Flow's Relay integration. For example

```
relay_integration=true
relay_integration.excludes=<PROJECT_ROOT>/dirA
relay_integration.excludes=<PROJECT_ROOT>/dirB
```

### suppress_type {#toc-suppress-type}

Type: `string`

This option lets you alias `any` with a given string. This is useful for
explaining why you're using `any`. For example, let's say you sometimes want
to sometimes use `any` to suppress an error and sometimes to mark a TODO.
Your code might look like

```
const myString: any = 1 + 1;
const myBoolean: any = 1 + 1;
```

If you add the following to your configuration:

```
[options]
suppress_type=$FlowFixMe
suppress_type=$FlowTODO
```

You can update your code to the more readable:

```
const myString: $FlowFixMe = 1 + 1;
const myBoolean: $FlowTODO = 1 + 1;
```

> **Note:** You can specify `suppress_type` multiple times.

### traces {#toc-traces}

Type: `integer`

Enables traces on all error output (showing additional details about the flow
of types through the system), to the depth specified. This can be very
expensive, so is disabled by default.

### use_mixed_in_catch_variables <SinceVersion version="0.201" /> {#toc-use-mixed-in-catch-variables}

Type: `boolean`

Changes the default type of `catch` variables from [`any`](../../types/any) to [`mixed`](../../types/mixed). E.g.

```js flow-check
try {
} catch (e) {
}
```

in the above example, if the option is `true`, `catch` will be typed as `mixed` as it lacks an explicit type annotation.

## Deprecated options

The following options no longer exist in the latest version of Flow:

### esproposal.class_instance_fields <UntilVersion version="0.148" /> {#toc-esproposal-class-instance-fields}

Type: `enable | ignore | warn`

Set this to `warn` to indicate that Flow should give a warning on use of
instance [class fields](https://github.com/tc39/proposal-class-public-fields)
per the pending spec.

You may also set this to `ignore` to indicate that Flow should simply ignore
the syntax (i.e. Flow will not use this syntax to indicate the presence of a
property on instances of the class).

The default value of this option is `enable`, which allows use of this proposed
syntax.

### esproposal.class_static_fields <UntilVersion version="0.148" /> {#toc-esproposal-class-static-fields}

Type: `enable | ignore | warn`

Set this to `warn` to indicate that Flow should give a warning on use of static
[class fields](https://github.com/tc39/proposal-class-public-fields)
per the pending spec.

You may also set this to `ignore` to indicate that Flow should simply ignore
the syntax (i.e. Flow will not use this syntax to indicate the presence of a
static property on the class).

The default value of this option is `enable`, which allows use of this proposed
syntax.

### esproposal.decorators <UntilVersion version="0.148" /> {#toc-esproposal-decorators}

Type: `ignore | warn`

Set this to `ignore` to indicate that Flow should ignore decorators.

The default value of this option is `warn`, which gives a warning on use since
this proposal is still very early-stage.

### esproposal.export_star_as <UntilVersion version="0.148" /> {#toc-esproposal-export-star-as}

Type: `enable | ignore | warn`

Set this to `enable` to indicate that Flow should support the `export * as`
syntax from [leebyron's proposal](https://github.com/leebyron/ecmascript-more-export-from).

You may also set this to `ignore` to indicate that Flow should simply ignore
the syntax. The default value of this option is `warn`, which gives a warning
on use since this proposal is still very early-stage.

### esproposal.optional_chaining <UntilVersion version="0.148" /> {#toc-esproposal-optional-chaining}

Type: `enable | ignore | warn`

Set this to `enable` to indicate that Flow should support the use of
[optional chaining](https://github.com/tc39/proposal-optional-chaining)
per the pending spec.

You may also set this to `ignore` to indicate that Flow should simply ignore
the syntax.

The default value of this option is `warn`, which gives a warning on
use since this proposal is still very early-stage.

### esproposal.nullish_coalescing <UntilVersion version="0.148" /> {#toc-esproposal-nullish-coalescing}

Type: `enable | ignore | warn`

Set this to `enable` to indicate that Flow should support the use of
[nullish coalescing](https://github.com/tc39/proposal-nullish-coalescing)
per the pending spec.

You may also set this to `ignore` to indicate that Flow should simply ignore
the syntax.

The default value of this option is `warn`, which gives a warning on
use since this proposal is still very early-stage.

### inference_mode <SinceVersion version="0.184.0" /> <UntilVersion version="0.202.0" /> {#toc-inference-mode}

Type: `classic | constrain-writes`

Setting this to `constrain-writes` will enable the constrained-writes inference mode.

For more info, see the [variable declaration docs](../../lang/variables).

The default value is `classic`

### log.file {#toc-log-file}

Type: `string`

The path to the log file (defaults to `/tmp/flow/<escaped root path>.log`).

### sharedmemory.dirs {#toc-sharedmemory-dirs}

Type: `string`

This affects Linux only.

Flow's shared memory lives in a memory mapped file. On more modern versions of
Linux (3.17+), there is a system call `memfd_create` which allows Flow to create
the file anonymously and only in memory. However, in older kernels, Flow needs
to create a file on the file system. Ideally this file lives on a memory-backed
tmpfs. This option lets you decide where that file is created.

By default this option is set to `/dev/shm` and `/tmp`

> **Note:** You can specify `sharedmemory.dirs` multiple times.

### sharedmemory.minimum_available {#toc-sharedmemory-minimum-available}

Type: `unsigned integer`

This affects Linux only.

As explained in the [`sharedmemory.dirs`](#toc-sharedmemory-dirs-string) option's description, Flow needs to
create a file on a filesystem for older kernels. `sharedmemory.dirs` specifies
a list of locations where the shared memory file can be created. For each
location, Flow will check to make sure the filesystem has enough space for the
shared memory file. If Flow will likely run out of space, it skips that location
and tries the next. This option lets you configure the minimum amount of space
needed on a filesystem for shared memory.

By default it is 536870912 (2^29 bytes, which is half a gigabyte).

### strip_root <UntilVersion version="0.48" /> {#toc-strip-root}

Type: `boolean`

Obsolete. Set this to `true` to always strip the root directory from file paths
in error messages when using `--json`, `--from emacs`, and `--from vim`.
Do not use this option. Instead, pass the command line flag `--strip-root`.

By default this is `false`.

### suppress_comment <UntilVersion version="0.126" /> {#toc-suppress-comment}

Type: `regex`

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

> **Note:** In version v0.127.0, the option to specify the suppression comment
> syntax was removed. `$FlowFixMe`, `$FlowIssue`, `$FlowExpectedError`,
> and `$FlowIgnore` became the only standard suppressions.

### temp_dir {#toc-temp-dir}

Type: `string`

Tell Flow which directory to use as a temp directory. Can be overridden with the
command line flag `--temp-dir`.

The default value is `/tmp/flow`.

### types_first <SinceVersion version="0.125.0" /> <UntilVersion version="0.142" /> {#toc-types-first}

Type: `boolean`

For more on types-first mode, see the [types-first docs](../../lang/types-first/).

Flow builds intermediate artifacts to represent signatures of modules as they are
checked. If this option is set to `false`, then these artifacts are built using
inferred type information. If this option is set to `true`, then they are built
using type annotations at module boundaries.

The default value for `types_first` is `true` (as of version 0.134).

### well_formed_exports <SinceVersion version="0.125.0" /> <UntilVersion version="0.142" /> {#toc-well-formed-exports}

Type: `boolean`

Enforce the following restrictions on file exports:
* Statements manipulating `module.exports` and the `exports` alias may only appear
  as top-level statements.
* Parts of the source that are visible from a file's exports need to be annotated
  unless their type can be trivially inferred (e.g. the exported expression is a
  numeric literal). This is a requirement for types-first mode to function properly.
  Failure to properly annotate exports raise `signature-verification-failure`s.

This option is set to `true` by default, since it is implied by [`types_first`](#toc-types-first-boolean),
but the option is useful on its own when upgrading a project from classic mode to
types-first mode.

### well_formed_exports.includes <SinceVersion version="0.128.0" /> <UntilVersion version="0.142" /> {#toc-well-formed-exports-includes}

Type: `string`

Limit the scope of the `well_formed_exports` requirement to a specific directory
of this project. For example
```
well_formed_exports=true
well_formed_exports.includes=<PROJECT_ROOT>/dirA
well_formed_exports.includes=<PROJECT_ROOT>/dirB
```
will only report export related errors in files under `dirA` and `dirB`. This option
requires `well_formed_exports` to be set to `true`.

The purpose of this option is to help prepare a codebase for Flow types-first mode.

Between versions v0.125.0 and v0.127.0, this option was named `well_formed_exports.whitelist`.
