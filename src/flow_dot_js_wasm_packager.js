/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

const fs = require('fs');
const path = require('path');
const zlib = require('zlib');

const input = process.argv[2];
const output = process.argv[3];
const postJs = process.argv[4];

if (input == null || output == null || postJs == null) {
  throw new Error(
    'Usage: flow_dot_js_wasm_packager.js <raw-js> <output-js> <post-js>',
  );
}

function getWasmSidecarPath(rawJsPath) {
  return path.join(
    path.dirname(rawJsPath),
    path.basename(rawJsPath, path.extname(rawJsPath)) + '.wasm',
  );
}

function runtimeShim() {
  return `var window = typeof window !== 'undefined'
  ? window
  : typeof globalThis !== 'undefined'
    ? globalThis
    : {};
var process = undefined;
var Module = typeof Module !== 'undefined' ? Module : {};
Module.noInitialRun = true;
Module.noExitRuntime = true;
Module.quit = function(_status, toThrow) {
  throw toThrow;
};
`;
}

function compressedWasmLoader(wasmPath) {
  if (!fs.existsSync(wasmPath)) {
    return '';
  }
  const wasm = fs.readFileSync(wasmPath);
  const compressed = zlib.deflateSync(wasm, {level: 9}).toString('base64');
  return `  var flowDotJsCompressedWasmBase64 = '${compressed}';
  var flowDotJsCompressedWasmBinaryPromise;
  function flowDotJsBase64ToBytes(base64) {
    if (typeof Buffer !== 'undefined') {
      return new Uint8Array(Buffer.from(base64, 'base64'));
    }
    var binary = atob(base64);
    var bytes = new Uint8Array(binary.length);
    for (var i = 0; i < binary.length; i++) {
      bytes[i] = binary.charCodeAt(i);
    }
    return bytes;
  }
  function flowDotJsInflateWasm(bytes) {
    if (typeof require === 'function') {
      try {
        var nodeZlib = require('zlib');
        return Promise.resolve(new Uint8Array(nodeZlib.inflateSync(Buffer.from(bytes))));
      } catch (_error) {}
    }
    if (
      typeof DecompressionStream === 'function' &&
      typeof Blob === 'function' &&
      typeof Response === 'function'
    ) {
      var stream = new Blob([bytes]).stream().pipeThrough(new DecompressionStream('deflate'));
      return new Response(stream).arrayBuffer().then(function(buffer) {
        return new Uint8Array(buffer);
      });
    }
    return Promise.reject(new Error('flow.js wasm requires DecompressionStream'));
  }
  function flowDotJsGetCompressedWasmBinary() {
    if (flowDotJsCompressedWasmBinaryPromise == null) {
      flowDotJsCompressedWasmBinaryPromise = flowDotJsInflateWasm(
        flowDotJsBase64ToBytes(flowDotJsCompressedWasmBase64)
      );
    }
    return flowDotJsCompressedWasmBinaryPromise;
  }
  // emcc 3.1.44 -O1 output has no Module.instantiateWasm hook, and its
  // createWasm runs synchronously at load time -- too early for an async inflate
  // to set wasmBinary first. But createWasm delegates to the (hoisted, global)
  // instantiateAsync, which this prefix can reassign before createWasm runs. So
  // we wrap instantiateAsync to ignore its fetch path and instead feed the
  // inflated bytes, awaiting the (async) decompression first. This works in Node
  // (zlib.inflateSync) and browsers (DecompressionStream) alike -- no vendored
  // inflater, no sidecar fetch, single self-contained file.
  var flowDotJsWasmBinaryPromise = flowDotJsGetCompressedWasmBinary();
  instantiateAsync = function(binary, binaryFile, imports, callback) {
    flowDotJsWasmBinaryPromise
      .then(function(realBinary) {
        return WebAssembly.instantiate(realBinary, imports);
      })
      .then(
        function(result) {
          callback({instance: result.instance, module: result.module});
        },
        function(error) {
          setTimeout(function() {
            throw error;
          }, 0);
        }
      );
  };
`;
}

const rawJs = fs.readFileSync(input, 'utf8');
const apiWrapper = fs.readFileSync(postJs, 'utf8');
const js =
  runtimeShim() +
  compressedWasmLoader(getWasmSidecarPath(input)) +
  rawJs +
  '\n' +
  apiWrapper +
  '\n';

fs.writeFileSync(output, js);
