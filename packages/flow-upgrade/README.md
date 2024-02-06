# `flow-upgrade`

A utility for upgrading your codebase to the latest version of Flow.

Install using `yarn add flow-upgrade`. We expect `prettier` to be installed as a [peer dependency](https://docs.npmjs.com/cli/v8/configuring-npm/package-json#peerdependencies) wherever you are running `flow-upgrade`.

You can then run:

```
yarn run flow-upgrade <current flow version> <target flow version>
```

You may also use [`npx`](https://www.npmjs.com/package/npx):

```
npx flow-upgrade <current flow version> <target flow version>
```

We also supply the `flow-codemod` binary:

```
yarn run flow-codemod <codemod name>
```

If you just want to run a codemod without specifying Flow versions.

## Options

### `all`

By default, Flow will only upgrade files that have an `// @flow` header comment.
If you want to upgrade all of your JavaScript files you may pass in the `--all`:

```
yarn create flow-upgrade --all
```

### `prettierrc`

Path to a `.prettierrc` file to use.
Upgrade codemods rely upon [`prettier`](https://www.npmjs.com/package/prettier) to print the resulting code after transformation.
If this is not provided, we will just use the defaults.

## Codemods available

### Collapse object initialization
Converts static object assignments (e.g. `const o = {}; o.a = 1;`) to inline properties (e.g. `const o = {a: 1};`).

Run with `yarn run flow-codemod collapseObjectInitialization`.

### Convert type parameter bound exact empty object to inexact
Replaces `T: {}` with `T: {...}` in type parameter bounds. The former is almost always wrong.

Run with `yarn run flow-codemod typeParameterBoundExactEmptyObjectToInexact`.

### Convert implicit inexact object types
Converts implicitly inexact object type syntax `{}` to explicitly inexact `{...}`.

Run with `yarn run flow-codemod convertImplicitInexactObjectTypes`.

### Remove explicitly exact object type syntax
Converts explicitly exact object type syntax `{| |}` to be just be `{ }`. To be done after you turn on `exact_by_default=true` in your `.flowconfig`.

Run with `yarn run flow-codemod removeExplicitlyExactObjectTypeSyntax`.

### Remove annotations in destructuring
Removes annotations nested inside of destructuring (e.g. `const [o: number] = foo;`). These are not valid Flow syntax.

Run with `yarn run flow-codemod removeAnnotationsInDestructuring`.

Part of the upgrade to 0.176

### Remove duplicate class properties
Removes useless duplicate class properties and fixes bad constructor binding in those classes.

Run with `yarn run flow-codemod removeDuplicateClassProperties`.

Part of the upgrade to 0.170

### Rename `$Partial` to `Partial`
Renames usages of the `$Partial` utility type to its new name, [`Partial`](https://flow.org/en/docs/types/utilities/#toc-partial).

Run with `yarn run flow-codemod renamePartial`.

Part of the upgrade to 0.201

### Convert `$Shape` to `Partial`
Converts usages of the deprecated and unsafe `$Shape` utility type to its replacement, [`Partial`](https://flow.org/en/docs/types/utilities/#toc-partial).

Run with `yarn run flow-codemod convertShapeToPartial`.

### Migrate type casts `(x: T)` to `as` expressions `x as T`
Converts usages of the old casting syntax `(x: T)` to the new casting syntax `x as T`.

Run with `yarn run flow-codemod typeCastToAsExpression`.
