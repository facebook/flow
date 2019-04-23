/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

var flowRemoveTypes = require('./index');

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
}

// Swizzle Module#_compile on each applicable module instance.
// NOTE: if using alongside Babel or another require-hook which simply
// over-writes the require.extensions and does not continue execution, then
// this require hook must come after it. Encourage those module authors to call
// the prior loader in their require hooks.
var jsLoader = require.extensions['.js'];
var exts = [ '.js', '.mjs', '.jsx', '.flow', '.es6' ];
exts.forEach(function (ext) {
  var superLoader = require.extensions[ext] || jsLoader;
  require.extensions[ext] = function (module, filename) {
    if (shouldTransform(filename, options)) {
      var super_compile = module._compile;
      module._compile = function _compile(code, filename) {
        try {
          var patched = flowRemoveTypes(code, options);
        }
        catch (e) {
          e.message = filename + ': ' + e.message;
          throw e;
        }
        super_compile.call(this, patched.toString(), filename);
      };
    }
    superLoader(module, filename);
  };
});

function shouldTransform(filename, options) {
  var includes = options && regexpPattern(options.includes || options.include);
  var excludes =
    options && 'excludes' in options ? regexpPattern(options.excludes) :
    options && 'exclude' in options ? regexpPattern(options.exclude) :
    /\/node_modules\//;
  return (!includes || includes.test(filename)) && !(excludes && excludes.test(filename));
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
    'flow-remove-types: includes and excludes must be RegExp or path strings. Got: ' + pattern
  );
}
