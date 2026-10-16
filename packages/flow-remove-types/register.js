/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

var Module = require('module');
var runnable = require('./runnable');
var pirates = require('pirates');

// Supported options:
//
//   - all: Transform all files, not just those with a @flow comment.
//   - includes: A Regexp/String to determine which files should be transformed.
//               (alias: include)
//   - excludes: A Regexp/String to determine which files should not be
//               transformed, defaults to ignoring /node_modules/, provide null
//               to exclude nothing. (alias: exclude)
var options;
// ES modules, including entry points that Node reloads after detecting module syntax, are
// loaded by the ESM loader rather than through the require hook below.
var esmOptions = Module.register ? runnable.registerESM(options) : null;
module.exports = function setOptions(newOptions) {
  options = newOptions;
  if (esmOptions) {
    runnable.writeOptions(esmOptions, newOptions);
  }
};

var exts = ['.js', '.mjs', '.cjs', '.jsx', '.flow', '.es6'];

var revert = pirates.addHook(
  function hook(code, filename) {
    try {
      return runnable.transform(code, filename, options);
    } catch (e) {
      e.message = filename + ': ' + e.message;
      throw e;
    }
  },
  {
    exts: exts,
    matcher: function (filename) {
      return runnable.shouldTransform(filename, options);
    },
  },
);
