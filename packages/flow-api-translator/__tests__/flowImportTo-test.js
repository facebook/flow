/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {MapperOptions} from '../src/flowImportTo';

// $FlowExpectedError[cannot-resolve-module]
import prettierConfig from '../../.prettierrc.json';
import {parse, print} from 'flow-transform';
import {trimToBeCode} from './utils/inlineCodeHelpers';
import {flowImportTo} from '../src/flowImportTo';

async function translate(code: string, opts: MapperOptions): Promise<string> {
  const {ast, scopeManager} = await parse(code);

  const flowDefAst = flowImportTo(ast, code, scopeManager, opts);

  return print(flowDefAst, code, prettierConfig);
}

async function expectModules(
  expectCode: string,
  toBeModules: Array<string>,
): Promise<void> {
  const foundModules = [];
  await translate(expectCode, {
    sourceMapper({module}) {
      foundModules.push(module);
      return module;
    },
  });
  expect(foundModules).toEqual(toBeModules);
}
async function expectTranslate(
  expectCode: string,
  opts: MapperOptions,
  toBeCode: string,
): Promise<void> {
  const expectTranslateCode = await translate(expectCode, opts);
  expect(expectTranslateCode).toBe(trimToBeCode(toBeCode));
}

describe('flowImportTo', () => {
  it('ImportDeclaration', async () => {
    await expectModules(
      `import A from 'X1';
       import * as B from 'X2';
       import {C} from 'X3';
       import type {D} from 'X4';
       import typeof {E} from 'X5';`,
      ['X1', 'X2', 'X3', 'X4', 'X5'],
    );
    await expectTranslate(
      `import A from 'X1';`,
      {sourceMapper: _ => 'A'},
      `import A from 'A';`,
    );
  });
  it('ExportNamedDeclaration', async () => {
    await expectModules(`export {A} from 'X1';`, ['X1']);
  });
  it('DeclareExportNamedDeclaration', async () => {
    await expectModules(`declare export {A} from 'X1';`, ['X1']);
  });
  it('ExportAllDeclaration', async () => {
    await expectModules(`export * from 'X1';`, ['X1']);
  });
  it('DeclareExportAllDeclaration', async () => {
    await expectModules(`declare export * from 'X1';`, ['X1']);
  });
});
