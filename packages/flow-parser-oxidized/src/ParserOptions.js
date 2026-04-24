/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

const ParserOptionsKeys = new Set([
  'allowReturnOutsideFunction',
  'babel',
  'flow',
  'enableExperimentalComponentSyntax',
  'enableExperimentalFlowMatchSyntax',
  'enableExperimentalFlowRecordSyntax',
  'reactRuntimeTarget',
  'sourceFilename',
  'sourceType',
  'tokens',
  'transformOptions',
]);

module.exports = {ParserOptionsKeys};
