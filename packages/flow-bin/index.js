/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

'use strict';

var VERSION = require('./package.json').version;

var path = require('path');

module.exports =
  process.platform === 'darwin'
    ? path.join(__dirname, 'flow-osx-arm64-v' + VERSION, 'flow') :
  process.platform === 'linux' && process.arch === 'x64'
    ? path.join(__dirname, 'flow-linux64-v' + VERSION, 'flow') :
  process.platform === 'linux' && process.arch === 'arm64'
    ? path.join(__dirname, 'flow-linux-arm64-v' + VERSION, 'flow') :
  process.platform === 'win32' &&  process.arch === 'x64'
    ? path.join(__dirname, 'flow-win64-v' + VERSION, 'flow.exe') :
  null;
