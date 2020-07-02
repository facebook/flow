---
layout: guide
---

Flow checks codebases by processing each file separately in dependency
order. After a file has been checked, a signature is extracted and stored in
main memory, to be used for files that depend on it. Currently, the default mode
(we'll also refer to it as *classic* mode) builds these signatures by using the
types inferred for the file's exports. In the new *types-first* architecture,
Flow relies on annotations available at the boundaries of files to build these
signatures.

The benefit of this new architecture is dual:

1. It dramatically improves *performance*, in particular when it comes to
rechecks. Suppose we want Flow to check a file `foo.js`, for which it hasn't
checked its dependencies yet. Classic mode would need to check all
dependencies and generate signatures from them first, before it could check
`foo.js`. In types-first, Flow extracts the dependency signatures just by
looking at the annotations around the exports. This process is mostly
syntactic, and therefore much faster than full type inference.

2. It improves error *reliability*. Inferred types often become complicated, and may
lead to errors being reported in downstream files, far away from their actual source.
Type annotations at file boundaries of files can help localize such errors, and
address them in the file that introduced them.

The caveat of this new version is that it requires exported parts of the code to be
annotated with types, or to be expressions whose type can be trivially inferred
(for example numbers and strings).

## How to upgrade your codebase to Types-First <a class="toc" id="toc-how-to-upgrade-your-codebase-to-types-first" href="#toc-how-to-upgrade-your-codebase-to-types-first"></a>

### Upgrade Flow version <a class="toc" id="toc-upgrade-flow-version" href="#toc-upgrade-flow-version"></a>

Types-first mode is officially released with version 0.125, but has been available in
*experimental* status as of version 0.102. If you are currently on an older
Flow version, you’d have to first upgrade Flow. Using the latest Flow version
is the best way to benefit from the performance benefits outlined above.

### Prepare your codebase for Types-First <a class="toc" id="toc-prepare-your-codebase-for-types-first" href="#toc-prepare-your-codebase-for-types-first"></a>

Types-first requires annotations at module boundaries in order to build type
signature for files. If these annotations are missing, then a `signature-verification-failure`
is raised, and the exported type for the respective part of the code will be `any`.

To see what types are missing to make your codebase types-first ready, add the
following line to the `[options]` section of the `.flowconfig` file:

```
well_formed_exports=true
```

Consider for example a file `foo.js` that exports a function call to `foo`

```js
declare function foo<T>(x: T): T;
module.exports = foo(1);
```

The return type of function calls is currently not trivially inferable (due to
features like polymorphism, overloading etc.). Their result needs to be annotated
and so you’d see the following error:

```
Cannot build a typed interface for this module. You should annotate the exports
of this module with types. Cannot determine the type of this call expression. Please
provide an annotation, e.g., by adding a type cast around this expression.
(`signature-verification-failure`)

   4│ module.exports = foo(1);
                       ^^^^^^
```

To resolve this, you can add an annotation like the following:

```js
declare function foo<T>(x: T): T;
module.exports = (foo(1): number);
```

#### Seal your intermediate results <a class="toc" id="toc-seal-your-intermediate-results" href="#toc-seal-your-intermediate-results"></a>

As you make progress adding types to your codebase, you can include directories
so that they don’t regress as new code gets committed, and until the entire project
has well-formed exports. You can do this by adding lines like the following to your
.flowconfig:

```
well_formed_exports.includes=<PROJECT_ROOT>/path/to/directory
```

> Warning: That this is a *substring* check, not a regular expression (for performance
reasons).


#### A codemod for large codebases <a class="toc" id="toc-a-codemod-for-large-codebases" href="#toc-a-codemod-for-large-codebases"></a>


Adding the necessary annotations to large codebases can be quite tedious. To ease
this burden, we are providing a codemod based on Flow's inference, that can be
used to annotate multiple files in bulk. See [this tutorial](../../cli/annotate-exports/) for more.


### Enable the types-first flag <a class="toc" id="toc-enable-the-types-first-flag" href="#toc-enable-the-types-first-flag"></a>

Once you have eliminated signature verification errors, you can turn on the types-first
mode, by adding the following line to the `[options]` section of the `.flowconfig` file:

```
types_first=true
```

You can also pass `--types-first` to the `flow check` or `flow start` commands.

The `well_formed_exports` flag from before is implied by `types_first`. Once
this process is completed and types-first has been enabled, you can remove
`well_formed_exports`.

Unfortunately, it is not possible to enable types-first mode for part of your repo; this switch
affects all files managed by the current `.flowconfig`.

> Note: The above flags are available in versions of Flow `>=0.102` with the `experimental.`
prefix (and prior to v0.128, it used `whitelist` in place of `includes`):
```
experimental.well_formed_exports=true
experimental.well_formed_exports.whitelist=<PROJECT_ROOT>/path/to/directory
experimental.types_first=true
```


### Deal with newly introduced errors <a class="toc" id="toc-deal-with-newly-introduced-errors" href="#toc-deal-with-newly-introduced-errors"></a>

Switching between classic and types-first mode may cause some new Flow errors,
besides signature-verification failures that we mentioned earlier. These errors
are due differences in the way types based on annotations are interpreted, compared
to their respective inferred types.

Below are some common error patterns and how to overcome them.


#### Array tuples treated as regular arrays in exports <a class="toc" id="toc-array-tuples-treated-as-regular-arrays-in-exports" href="#toc-array-tuples-treated-as-regular-arrays-in-exports"></a>


In types-first, an array literal in an *export position*

```js
module.exports = [e1, e2];
```

is treated as having type `Array<t1 | t2>`, where `e1` and `e2` have types `t1`
and `t2`, instead of the tuple type `[t1, t2]`.

In classic mode, the inferred type encompassed both types at the same time. This
might cause errors in importing files that expect for example to find type `t1`
in the first position of the import.

**Fix:** If a tuple type is expected, then the annotation `[t1, t2]` needs to be
explicitly added on the export side.

#### Indirect object assignments in exports <a class="toc" id="toc-indirect-object-assignments-in-exports" href="#toc-indirect-object-assignments-in-exports"></a>


Flow allows the code

```js
function foo(): void {}
foo.x = () => {};
foo.x.y = 2;
module.exports = foo;
```

but in types-first the exported type will be

```plaintext
{
  (): void;
  x: () => void;
}
```

In other words it won’t take into account the update on `y`.

**Fix:** To include the update on `y` in the exported type, the export will need
to be annotated with the type

```plaintext
{
  (): void;
  x: { (): void; y: number; };
};
```

The same holds for more complex assignment patterns like

```js
function foo(): void {}
Object.assign(foo, { x: 1});
module.exports = foo;
```

where you’ll need to manually annotate the export with `{ (): void; x: number }`,
or assignments preceding the function definition

```js
foo.x = 1;
function foo(): void {}
module.exports = foo;
```

Note that in the last example, Flow types-first will pick up the static update if
it was after the definition:

```js
function foo(): void {}
foo.x = 1;
module.exports = foo;
```

### Exported variables with updates <a class="toc" id="toc-exported-variables-with-updates" href="#toc-exported-variables-with-updates"></a>

The types-first signature extractor will not pick up subsequent update of an exported
let-bound variables. Consider the example

```js
let foo: number | string = 1;
foo = "blah";
module.exports = foo;
```

In classic mode the exported type would be `string`. In types-first it will be
`number | string`, so if downstream typing depends on the more precise type, then
you might get some errors.

**Fix:** Introduce a new variable on the update and export that one. For example
```js
const foo1: number | string = 1;
const foo2 = "blah";
module.exports = foo2;
```
