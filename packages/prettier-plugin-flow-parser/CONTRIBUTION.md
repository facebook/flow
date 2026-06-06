# Modified prettier v3

This directory contains a modified version of prettier v3's Hermes plugin.

For internal Meta release process and detailed development docs, see [facebook/INTERNAL_DEV.md](facebook/INTERNAL_DEV.md).

## Recreating prettier files

To make changes to our fork of prettier v3:

1. Run yarn build script: `hermes-parser/js$ ./scripts/build-prettier.sh`
2. Make any modifications to the prettier git repo inside `hermes-parser/js/prettier-hermes-flow-fork`
3. Run `hermes-parser/js$ yarn build-prettier` after making any changes.
4. Once you're done making changes, commit inside the `prettier-hermes-flow-fork` git repo, and push your changes to the [upstream repo](https://github.com/pieterv/prettier/tree/flow-fork)
5. If relevant, put up a PR with your changes to https://github.com/prettier/prettier

### Manual process

1. Check out prettier fork: `https://github.com/pieterv/prettier/tree/flow-fork`.
2. Build repo: `yarn build --package=@prettier/plugin-hermes`
3. Copy built files `dist/plugin-hermes` and all JS files over there to this location.
