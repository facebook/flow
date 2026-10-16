// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

'use strict';

var emitter = require('./runnable-wasm');
var Module = require('module');
var fs = require('fs');
var path = require('path');
var fileURLToPath = require('url').fileURLToPath;
var pathToFileURL = require('url').pathToFileURL;
process.setSourceMapsEnabled(true);
var sourceMaps = new Map();
require('source-map-support').install({
  environment: 'node',
  handleUncaughtExceptions: false,
  emptyCacheBetweenOperations: true,
  retrieveSourceMap: function (filename) {
    var nativeMap = Module.findSourceMap(filename);
    var map = sourceMaps.get(filename);
    if (!map && nativeMap) {
      map = Object.assign({}, nativeMap.payload, {
        sources: nativeMap.payload.sources.map(function (source) {
          return source.startsWith('file:') ? fileURLToPath(source) : source;
        })
      });
    }
    var url = filename.startsWith('file:') ? fileURLToPath(filename) : path.isAbsolute(filename) ? filename : null;
    return map ? { url: url, map: map } : null;
  }
});

exports.setSourceMap = function (filename, map) {
  sourceMaps.set(filename, map);
};

// Options reach the loader thread through shared memory: transferring a MessagePort to
// `module.register` from a `--require` preload kills Node's hooks thread.
exports.writeOptions = function (shared, options) {
  var json = JSON.stringify(options === undefined ? null : options, function (_key, value) {
    return value instanceof RegExp ? {$regexp: value.source, flags: value.flags} : value;
  });
  var bytes = Buffer.from(json, 'utf8');
  if (bytes.length > shared.byteLength - 4) {
    throw new Error('flow-remove-types: options are too large to share with the ES module loader');
  }
  new Uint8Array(shared, 4).set(bytes);
  Atomics.store(new Int32Array(shared, 0, 1), 0, bytes.length);
};

exports.readOptions = function (shared) {
  var length = Atomics.load(new Int32Array(shared, 0, 1), 0);
  var json = Buffer.from(new Uint8Array(shared, 4, length)).toString('utf8');
  var options = JSON.parse(json, function (_key, value) {
    return value != null && typeof value.$regexp === 'string' ? new RegExp(value.$regexp, value.flags) : value;
  });
  return options === null ? undefined : options;
};

// Returns the shared options that `writeOptions` updates.
exports.registerESM = function (options) {
  var shared = new SharedArrayBuffer(1 << 16);
  exports.writeOptions(shared, options);
  Module.register(pathToFileURL(require.resolve('./loader.mjs')), {
    data: {shared: shared}
  });
  return shared;
};

exports.sourceType = function (filename) {
  var extension = path.extname(filename);
  if (extension === '.mjs') return 'module';
  if (extension === '.cjs') return 'script';
  var directory = path.dirname(filename);
  while (true) {
    var packageFile = path.join(directory, 'package.json');
    if (fs.existsSync(packageFile)) {
      return JSON.parse(fs.readFileSync(packageFile, 'utf8')).type === 'module' ? 'module' : 'script';
    }
    var parent = path.dirname(directory);
    if (parent === directory) return 'script';
    directory = parent;
  }
};

exports.compile = function (source, filename, options, sourceType) {
  if (!(options && options.all) && source.indexOf('@flow') === -1 && source.indexOf('@noflow') === -1) {
    return {code: source, map: null};
  }
  var runtime = require.resolve('flow-enums-runtime');
  return emitter.flowToRunnableJSWithSourceMap(source, {
    sourceFilename: filename,
    sourceType: sourceType || 'script',
    enumRuntimeModule: sourceType === 'module' ? require('url').pathToFileURL(runtime).href : runtime,
    allowReturnOutsideFunction: sourceType !== 'module' && !(options && options.allowReturnOutsideFunction === false),
    reactRuntimeTarget: options && options.reactRuntimeTarget
  });
};

exports.inlineSourceMap = function (emitted) {
  var map = Object.assign({}, emitted.map, {
    sources: emitted.map.sources.map(function (source) {
      return path.isAbsolute(source) ? pathToFileURL(source).href : source;
    })
  });
  return emitted.code + '\n//# sourceMappingURL=data:application/json;base64,' +
    Buffer.from(JSON.stringify(map)).toString('base64');
};

exports.transform = function (source, filename, options) {
  var emitted = exports.compile(source, filename, options, 'script');
  if (emitted.map) {
    exports.setSourceMap(filename, emitted.map);
  }
  return emitted.map ? exports.inlineSourceMap(emitted) : emitted.code;
};

exports.shouldTransform = shouldTransform;

function shouldTransform(filename, options) {
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
