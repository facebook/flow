/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @noflow
 * @format
 */

'use strict';

const prettierFlowPlugin = require('prettier/plugins/flow');

module.exports = {
  __flowTransformUseFlowParserName: true,
  parsers: {
    hermes: prettierFlowPlugin.parsers.flow,
  },
};
