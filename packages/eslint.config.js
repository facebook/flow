/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

const {defineConfig, globalIgnores} = require('eslint/config');
const js = require('@eslint/js');
const globals = require('globals');
const eslintComments = require('@eslint-community/eslint-plugin-eslint-comments/configs');
const fbFlow = require('eslint-plugin-fb-flow');
const ftFlow = require('eslint-plugin-ft-flow');
const jest = require('eslint-plugin-jest');
const prettier = require('eslint-config-prettier/flat');

module.exports = defineConfig([
  globalIgnores([
    'flow-typed',
    'node_modules',
    '**/dist/**/*',
    '!**/dist/**/*.js.flow',
    '**/fixtures/**',
    'internal-prettier-v3/',
    'jest.config.js',
    'babel-plugin-transform-flow-enums/',
    'eslint-plugin-fb-flow/',
    'flow-dev-tools/',
    'flow-enums-runtime/',
    'flow-for-vscode/',
    'flow-parser/',
    'flow-remove-types/',
    'flow-typed-oxidized/',
    'flow-upgrade/',
    'try-flow-website-js/',
  ]),
  js.configs.recommended,
  eslintComments.recommended,
  prettier,
  {
    files: ['**/*.js', '**/*.js.flow'],
    languageOptions: {
      parser: require('flow-eslint'),
      globals: {
        ...globals.commonjs,
        ...globals.es2021,
        ...globals.node,
      },
    },
    plugins: {
      'fb-flow': fbFlow,
      'ft-flow': ftFlow,
    },
    settings: {
      'ft-flow': {
        onlyFilesWithFlowAnnotation: false,
      },
    },
    rules: {
      ...ftFlow.configs.recommended.rules,
      curly: ['error', 'all'],
      eqeqeq: ['error', 'always', {null: 'never'}],
      'no-unused-vars': [
        'error',
        {
          args: 'all',
          varsIgnorePattern: '^_',
          argsIgnorePattern: '^_',
          ignoreRestSiblings: true,
        },
      ],

      '@eslint-community/eslint-comments/no-unused-disable': 'error',

      'fb-flow/flow-enums-default-if-possible': 'error',
      'fb-flow/no-flow-enums-object-mapping': 'error',
      'fb-flow/use-exact-by-default-object-type': 'error',
      'fb-flow/use-indexed-access-type': 'error',

      'ft-flow/array-style-complex-type': ['error', 'verbose'],
      'ft-flow/array-style-simple-type': ['error', 'verbose'],

      'no-undef': 'off',
      'ft-flow/define-flow-type': 'off',

      // from eslint-config-prettier, but for ft-flow instead of flowtype
      'ft-flow/boolean-style': 'off',
      'ft-flow/delimiter-dangle': 'off',
      'ft-flow/generic-spacing': 'off',
      'ft-flow/object-type-curly-spacing': 'off',
      'ft-flow/object-type-delimiter': 'off',
      'ft-flow/quotes': 'off',
      'ft-flow/semi': 'off',
      'ft-flow/space-after-type-colon': 'off',
      'ft-flow/space-before-generic-bracket': 'off',
      'ft-flow/space-before-type-colon': 'off',
      'ft-flow/union-intersection-spacing': 'off',
    },
  },
  {
    files: ['**/__tests__/**'],
    languageOptions: {
      globals: {
        ...globals.jest,
      },
    },
    plugins: {
      jest,
    },
    settings: {
      version: require('jest/package.json').version,
    },
  },
  {
    files: ['**/__tests__/**/*-test.js'],
    plugins: {
      jest,
    },
    rules: {
      ...jest.configs.recommended.rules,
      // we have tests which use utils which don't inline expects
      'jest/expect-expect': 'off',
      'jest/valid-title': 'off',
      'jest/no-disabled-tests': 'off',
    },
  },
]);
