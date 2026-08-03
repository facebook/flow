/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

'use strict';

import type {Identifier, JSXIdentifier} from 'flow-estree';
import type {ScopeManager, Variable} from 'flow-eslint';

export type Dep = string;
export type TranslationOptions = {
  recoverFromErrors: boolean,
  mungeUnderscores?: boolean,
};
export type TranslationContext = {
  scopeManager: ScopeManager,
  referenceMap: Map<Identifier | JSXIdentifier, Variable>,
  variableMap: Map<Dep, Variable>,
  recoverFromErrors: boolean,
  mungeUnderscores: boolean,
  code: string,
};

export function createTranslationContext(
  code: string,
  scopeManager: ScopeManager,
  {recoverFromErrors, mungeUnderscores = true}: TranslationOptions,
): TranslationContext {
  const referenceMap = new Map<Identifier | JSXIdentifier, Variable>();
  const variableMap = new Map<Dep, Variable>();
  const moduleScope = scopeManager.globalScope.childScopes[0];
  if (moduleScope == null || moduleScope.type !== 'module') {
    throw new Error('createTranslationContext: Module scope not found');
  }
  for (const variable of moduleScope.variables) {
    for (const reference of variable.references) {
      referenceMap.set(reference.identifier, variable);
      variableMap.set(variable.name, variable);
    }
  }
  return {
    scopeManager,
    referenceMap,
    variableMap,
    recoverFromErrors,
    mungeUnderscores,
    code,
  };
}
