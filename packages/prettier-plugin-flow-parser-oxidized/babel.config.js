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
};
