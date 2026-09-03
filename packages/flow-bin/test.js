#!/usr/bin/env node
/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

var fs = require('fs');

// osx
process.platform = 'darwin';
process.arch = 'arm64'
delete require.cache[require.resolve('./')];
fs.statSync(require('./'));

// linux x64
process.platform = 'linux';
process.arch = 'x64'
delete require.cache[require.resolve('./')];
fs.statSync(require('./'));

// linux arm64
process.platform = 'linux';
process.arch = 'arm64'
delete require.cache[require.resolve('./')];
fs.statSync(require('./'));

// windows
process.platform = 'win32';
process.arch = 'x64'
delete require.cache[require.resolve('./')];
fs.statSync(require('./'));
