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

import type {Visitor} from '../traverse/traverse';
import type {TransformContextAdditions} from './TransformContext';

import {transformAST} from './transformAST';
import {parse} from './parse';
import {print} from './print';

export type TransformVisitor = Visitor<TransformContextAdditions>;

export async function transform(
  originalCode: string,
  visitors: TransformVisitor,
  prettierOptions: {...} = {},
): Promise<string> {
  const parseResult = parse(originalCode);

  const {ast, astWasMutated, mutatedCode} = transformAST(parseResult, visitors);
  if (!astWasMutated) {
    return originalCode;
  }

  return print(ast, mutatedCode, prettierOptions);
}
