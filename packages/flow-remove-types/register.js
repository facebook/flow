/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

var flowRemoveTypes = require('./index');
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
module.exports = function setOptions(newOptions) {
  options = newOptions;
};

var jsLoader = require.extensions['.js'];
var exts = ['.js', '.mjs', '.cjs', '.jsx', '.flow', '.es6'];

var revert = pirates.addHook(
  function hook(code, filename) {
    try {
      return flowRemoveTypes(code, options).toString();
    } catch (e) {
      e.message = filename + ': ' + e.message;
      throw e;
    }
  },
  {exts: exts, matcher: shouldTransform},
);

function shouldTransform(filename) {
  var includes = options && regexpPattern(options.includes || options.include);
  var excludes =
    options && 'excludes' in options
      ? regexpPattern(options.excludes)
      : options && 'exclude' in options
      ? regexpPattern(options.exclude)
      : /\/node_modules\//;
  return (
    (!includes || includes.test(filename)) &&
    !(excludes && excludes.test(filename))
  );
}

// Given a null | string | RegExp | any, returns null | Regexp or throws a
// more helpful error.
function regexpPattern(pattern) {
  if (!pattern) {
    return pattern;
  }
  // A very simplified glob transform which allows passing legible strings like
  // "myPath/*.js" instead of a harder to read RegExp like /\/myPath\/.*\.js/.
  if (typeof pattern === 'string') {
    pattern = pattern.replace(/\./g, '\\.').replace(/\*/g, '.*');
    if (pattern[0] !== '/') {
      pattern = '/' + pattern;
    }
    return new RegExp(pattern);
  }
  if (typeof pattern.test === 'function') {
    return pattern;
  }
  throw new Error(
    'flow-remove-types: ' +
      'includes and excludes must be RegExp or path strings. Got: ' +
      pattern,
  );
}
