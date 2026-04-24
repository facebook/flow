/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

import type {Comment} from 'flow-estree-oxidized';

import {VariableBase} from './VariableBase';

/**
 * ESLint defines global variables using the eslint-scope Variable class
 * This is declared here for consumers to use
 */
class ESLintScopeVariable extends VariableBase {
  /**
   * Written to by ESLint.
   * If this key exists, this variable is a global variable added by ESLint.
   * If this is `true`, this variable can be assigned arbitrary values.
   * If this is `false`, this variable is readonly.
   * @public
   */
  writeable: void | boolean; // note that this isn't a typo - ESlint uses this spelling here

  /**
   * Written to by ESLint.
   * This property is undefined if there are no globals directive comments.
   * The array of globals directive comments which defined this global variable in the source code file.
   * @public
   */
  eslintExplicitGlobal: void | boolean;

  /**
   * Written to by ESLint.
   * The configured value in config files. This can be different from `variable.writeable` if there are globals directive comments.
   * @public
   */
  eslintImplicitGlobalSetting: void | 'readonly' | 'writable';

  /**
   * Written to by ESLint.
   * If this key exists, it is a global variable added by ESLint.
   * @public
   */
  eslintExplicitGlobalComments: void | $ReadOnlyArray<Comment>;
}

export {ESLintScopeVariable};
