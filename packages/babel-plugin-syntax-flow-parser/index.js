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

// The plugin now lives in the `flow-parser` package and is importable via
// `flow-parser/babel-plugin`. This package is kept as a thin re-export so the
// published `babel-plugin-syntax-flow-parser` name continues to work.
module.exports = require('flow-parser/babel-plugin');
