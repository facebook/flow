# `flow-upgrade`

A utility for upgrading your codebase to the latest version of Flow.

To run this utility to upgrade your codebase you can use [`yarn create`][]:

[`yarn create`]: https://yarnpkg.com/en/docs/cli/create#search

```
yarn create flow-upgrade
```

This is a shorter version which is equivalent to:

```
yarn global add flow-upgrade
flow-upgrade
```

You may also use [`npx`][]:

[`npx`]: https://www.npmjs.com/package/npx

```
npx flow-upgrade
```

## Options

By default, Flow will only upgrade files that have an `// @flow` header comment.
If you want to upgrade all of your JavaScript files you may pass in the `--all`:

```
yarn create flow-upgrade --all
```
