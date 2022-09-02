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
Upgrade codemods rely upon [`prettier`][] to print the resulting code after transformation.
If this is not provided, we will just use the defaults.

[`prettier`]: https://www.npmjs.com/package/prettier
