/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// NOTE: using babel.config.js instead of .babelrc to force
// babel to compile files inside lib/pkg/* using this config
module.exports = function config(api) {
  api.env();

  const isEnvTest = process.env.NODE_ENV === 'test';

  return {
    plugins: ['@babel/plugin-proposal-class-properties'],

    presets: [
      [
        '@babel/preset-env',
        {
          targets: {
            node: '10',
          },
          modules: isEnvTest ? 'commonjs' : false,
        },
      ],
      '@babel/preset-flow',
    ],

    ignore: ['node_modules'],
  };
};
