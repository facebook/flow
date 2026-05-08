/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

declare module 'espree' {
  declare export function parse(
    code: string,
    options?: $ReadOnly<{
      /**
       * attach range information to each node
       */
      range?: boolean,
      /**
       * attach line/column location information to each node
       */
      loc?: boolean,
      /**
       * create a top-level comments array containing all comments
       */
      comment?: boolean,
      /**
       * create a top-level tokens array containing all tokens
       */
      tokens?: boolean,
      /**
       * Set to 3, 5 (the default), 6, 7, 8, 9, 10, 11, 12, or 13 to specify the version of ECMAScript syntax you want to use.
       * You can also set to 2015 (same as 6), 2016 (same as 7), 2017 (same as 8), 2018 (same as 9), 2019 (same as 10), 2020 (same as 11), 2021 (same as 12), or 2022 (same as 13) to use the year-based naming.
       * You can also set "latest" to use the most recently supported version.
       */
      ecmaVersion?:
        | 3
        | 5
        | 6
        | 7
        | 8
        | 9
        | 10
        | 11
        | 12
        | 13
        | 2015
        | 2016
        | 2017
        | 2018
        | 2019
        | 2020
        | 2021
        | 2022
        | 'latest',
      /**
       * only allowed when ecmaVersion is 3
       */
      allowReserved?: boolean,
      /**
       * specify which type of script you're parsing ("script", "module", or "commonjs")
       */
      sourceType?: 'script' | 'module' | 'commonjs',
      /**
       * specify additional language features
       */
      ecmaFeatures?: $ReadOnly<{
        /**
         * enable JSX parsing
         */
        jsx?: boolean,
        /**
         * enable return in global scope (set to true automatically when sourceType is "commonjs")
         */
        globalReturn?: boolean,
        /**
         * enable implied strict mode (if ecmaVersion >= 5)
         */
        impliedStrict?: boolean,
      }>,
    }>,
  ): mixed /* AST */;
}
