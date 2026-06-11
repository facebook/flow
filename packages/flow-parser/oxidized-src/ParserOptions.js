/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @noformat
 */

'use strict';

import type {Expression} from 'flow-estree';

export type ParserOptions = {
  allowReturnOutsideFunction?: boolean,
  assertOperator?: boolean,
  babel?: boolean,
  flow?: 'all' | 'detect',
  enableExperimentalComponentSyntax?: boolean,
  enableExperimentalDecorators?: boolean,
  enableExperimentalFlowMatchSyntax?: boolean,
  enableExperimentalFlowRecordSyntax?: boolean,
  enableEnums?: boolean,
  enableRecords?: boolean,
  enableTypes?: boolean,
  enableTypesPragmaDetection?: boolean,
  reactRuntimeTarget?: '18' | '19',
  sourceFilename?: string,
  sourceType?: 'module' | 'script' | 'unambiguous',
  throwOnParseErrors?: boolean,
  tokens?: boolean,
  transformOptions?: {
    +TransformEnumSyntax?: {
      +enable: boolean,
      +getRuntime?: () => Expression,
    },
  },
};

export const ParserOptionsKeys /*: ReadonlySet<keyof ParserOptions> */ = new Set([
  'allowReturnOutsideFunction',
  'assertOperator',
  'babel',
  'flow',
  'enableExperimentalComponentSyntax',
  'enableExperimentalFlowMatchSyntax',
  'enableExperimentalFlowRecordSyntax',
  'reactRuntimeTarget',
  'sourceFilename',
  'sourceType',
  'throwOnParseErrors',
  'tokens',
  'transformOptions',
]);
