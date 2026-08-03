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

import type {MapperOptions} from './flowImportTo';

import {parse, print} from 'flow-transform';
import {parse as parseTS} from '@typescript-eslint/parser';
import {visitorKeys as tsVisitorKeys} from '@typescript-eslint/visitor-keys';
import flowToFlowDef from './flowToFlowDef';
import {flowDefToTSDef} from './flowDefToTSDef';
import {flowToJS} from './flowToJS';
import {flowImportTo} from './flowImportTo';
import {TSDefToFlowDef} from './TSDefToFlowDef';

export async function translateFlowToFlowDef(
  code: string,
  prettierOptions: {...} = {},
  opts?: {mungeUnderscores?: boolean},
): Promise<string> {
  const {ast, scopeManager} = await parse(code);

  const [flowDefAst, mutatedCode] = flowToFlowDef(ast, code, scopeManager, {
    recoverFromErrors: true,
    mungeUnderscores: opts?.mungeUnderscores,
  });

  return print(flowDefAst, mutatedCode, prettierOptions);
}

export async function translateFlowToTSDef(
  code: string,
  prettierOptions: {...} = {},
): Promise<string> {
  const flowDefCode = await translateFlowToFlowDef(code, prettierOptions);

  return translateFlowDefToTSDef(flowDefCode, prettierOptions);
}

export async function translateFlowDefToTSDef(
  code: string,
  prettierOptions: {...} = {},
): Promise<string> {
  const {ast, scopeManager} = await parse(code);
  const [tsAST, mutatedCode] = flowDefToTSDef(code, ast, scopeManager, {
    recoverFromErrors: true,
  });

  return print(
    // $FlowExpectedError[incompatible-type] - this is fine as we're providing the visitor keys
    tsAST,
    mutatedCode,
    {
      ...prettierOptions,
    },
    tsVisitorKeys,
  );
}

export async function translateFlowToJS(
  code: string,
  prettierOptions: {...} = {},
): Promise<string> {
  const {ast, scopeManager} = await parse(code);

  const jsAST = flowToJS(ast, code, scopeManager);

  return print(jsAST, code, prettierOptions);
}

/**
 * This translator is very experimental and unstable.
 *
 * It is not written with productionizing it in mind, but instead used to evaluate how close Flow
 * is to TypeScript.
 *
 * If you are going to use it anyways, you agree that you are calling a potentially broken function
 * without any guarantee.
 *
 * @deprecated
 */
export async function unstable_translateTSDefToFlowDef(
  code: string,
  prettierOptions: {...} = {},
): Promise<string> {
  const ast = parseTS(code, {loc: true, range: true, sourceType: 'module'});
  if (ast == null) {
    throw `Failed to parse ${code} with @typescript-eslint/parser`;
  }
  const [flowAST, mutatedCode] = TSDefToFlowDef(code, ast, {
    recoverFromErrors: false,
  });

  return print(flowAST, mutatedCode, prettierOptions);
}

export type {MapperOptions as FlowImportsMapperOptions};
export async function translateFlowImportsTo(
  code: string,
  prettierOptions: {...} = {},
  opts: MapperOptions,
): Promise<string> {
  const {ast, scopeManager} = await parse(code);

  const jsAST = flowImportTo(ast, code, scopeManager, opts);

  return print(jsAST, code, prettierOptions);
}
