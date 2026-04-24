/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {TransformVisitor} from '../../src/transform/transform';

import {transform as transformOriginal} from '../../src/transform/transform';
// $FlowExpectedError[cannot-resolve-module]
import prettierConfig from '../../../.prettierrc.json';

export async function transform(
  code: string,
  visitors: TransformVisitor,
): Promise<string> {
  return transformOriginal(code, visitors, prettierConfig);
}

export * as t from '../../src/generated/node-types';
