// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

'use strict';

var FlowParserWASMModule = require('flow-parser/dist/FlowParserWASM');
var bindings;

function init() {
  if (bindings) return bindings;
  var wasm = FlowParserWASMModule({
    quit: function (_status, error) {
      throw error;
    }
  });
  function numberResult(name) {
    return wasm.cwrap(name, 'number', ['number']);
  }
  bindings = {
    wasm: wasm,
    emit: wasm.cwrap('flow_to_runnable_js_with_source_map', 'number', [
      'number', 'number', 'number', 'number', 'number', 'number', 'number'
    ]),
    free: wasm.cwrap('hermesParseResult_free', 'void', ['number']),
    error: wasm.cwrap('hermesParseResult_getError', 'string', ['number']),
    line: numberResult('hermesParseResult_getErrorLine'),
    column: numberResult('hermesParseResult_getErrorColumn'),
    program: numberResult('hermesParseResult_getProgramBuffer'),
    strings: numberResult('hermesParseResult_getStringBuffer')
  };
  return bindings;
}

exports.flowToRunnableJSWithSourceMap = function (source, options) {
  if (typeof options.sourceFilename !== 'string') {
    throw new TypeError('sourceFilename must be a string');
  }
  if (options.sourceType != null && options.sourceType !== 'script' && options.sourceType !== 'module') {
    throw new TypeError('sourceType must be "script" or "module"');
  }
  var api = init();
  var wasm = api.wasm;
  var sourceBuffer = Buffer.from(source, 'utf8');
  var filenameBuffer = Buffer.from(JSON.stringify({
    sourceFilename: options.sourceFilename,
    enumRuntimeModule: options.enumRuntimeModule
  }), 'utf8');
  var sourceAddr = 0;
  var filenameAddr = 0;
  var filenameLength = 0;
  var result = 0;
  try {
    sourceAddr = wasm._malloc(sourceBuffer.length + 1);
    if (!sourceAddr) throw new Error('Emitter out of memory');
    wasm.HEAP8.set(sourceBuffer, sourceAddr);
    wasm.HEAP8[sourceAddr + sourceBuffer.length] = 0;
    if (filenameBuffer !== null) {
      filenameAddr = wasm._malloc(filenameBuffer.length + 1);
      if (!filenameAddr) throw new Error('Emitter out of memory');
      wasm.HEAP8.set(filenameBuffer, filenameAddr);
      wasm.HEAP8[filenameAddr + filenameBuffer.length] = 0;
      filenameLength = filenameBuffer.length + 1;
    }
    result = api.emit(
      sourceAddr,
      sourceBuffer.length + 1,
      filenameAddr,
      filenameLength,
      options.sourceType === 'module' ? 2 : options.sourceType === 'script' ? 1 : 0,
      options.reactRuntimeTarget === '19' ? 19 : 18,
      options.allowReturnOutsideFunction === true ? 1 : 0
    );
    var error = api.error(result);
    if (error) {
      var line = api.line(result);
      var column = api.column(result);
      if (line > 0) {
        var syntaxError = new SyntaxError(options.sourceFilename + ':' + line + ':' + column + ': ' + error);
        syntaxError.loc = {line: line, column: column};
        throw syntaxError;
      }
      throw new Error(error);
    }
    var buffer = api.strings(result);
    var length = wasm.HEAPU32[api.program(result) >>> 2];
    return JSON.parse(Buffer.from(wasm.HEAP8.subarray(buffer, buffer + length)).toString('utf8'));
  } finally {
    if (result !== 0) api.free(result);
    if (sourceAddr !== 0) wasm._free(sourceAddr);
    if (filenameAddr !== 0) wasm._free(filenameAddr);
  }
};
