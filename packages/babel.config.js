/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

module.exports = {
  assumptions: {
    constantReexports: true,
    constantSuper: true,
    noClassCalls: true,
    noDocumentAll: true,
    noNewArrows: true,
    setPublicClassFields: true,
  },
  presets: [
    [
      '@babel/preset-env',
      {
        targets: {
          node: '12.0.0',
        },
      },
    ],
  ],
  plugins: [
    ['@babel/plugin-syntax-flow', {enums: true}],
    'babel-plugin-transform-flow-enums',
    ['@babel/plugin-transform-flow-strip-types', {allowDeclareFields: true}],
    '@babel/plugin-proposal-class-properties',
  ],
  overrides: overrideEnabled()
    ? [
        {
          // Use flow-parser-oxidized as Babel's parser so it understands newer
          // Flow syntax (e.g. `as` casts) beyond what the bundled @babel/parser
          // supports. Disabled by `SKIP_HERMES_PARSER_OVERRIDE=1` during the
          // build's bootstrap phase — `babel-plugin-syntax-flow-parser-oxidized`
          // and its workspace dependencies (`flow-parser-oxidized`,
          // `flow-estree-oxidized`) have to be built before this plugin can be
          // required.
          test: filename =>
            filename != null &&
            !filename.includes(
              '/babel-plugin-syntax-flow-parser-oxidized/__tests__/',
            ),
          plugins: ['babel-plugin-syntax-flow-parser-oxidized'],
        },
      ]
    : [],
};

function overrideEnabled() {
  if (process.env.SKIP_HERMES_PARSER_OVERRIDE) {
    return false;
  }
  // Fresh checkouts won't have the plugin built yet; skip rather than crash so
  // the build script can bootstrap it.
  const path = require('path');
  const fs = require('fs');
  return fs.existsSync(
    path.resolve(
      __dirname,
      'babel-plugin-syntax-flow-parser-oxidized/dist/index.js',
    ),
  );
}
