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

const OUTPUT_FILE = path.resolve(
  __dirname,
  '../flow-parser/oxidized/FlowParserWASM.js',
);

const HEADER = `/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

'use strict';
`;

// Emscripten 4 returns a Promise from modularized factories, but sync
// compilation still initializes the incoming module object before returning.
const FOOTER = `

const __flowParserWASMModuleFactory = module.exports;
module.exports = function flowParserWASMModuleFactory(moduleArg = {}) {
  const moduleResult = __flowParserWASMModuleFactory(moduleArg);
  if (moduleResult != null && typeof moduleResult.cwrap === 'function') {
    return moduleResult;
  }
  if (moduleArg != null && typeof moduleArg.cwrap === 'function') {
    return moduleArg;
  }
  throw new Error(
    'FlowParserWASM must initialize synchronously and export cwrap.',
  );
};
module.exports.default = module.exports;
`;

// Add header and sign file before writing back to disk
const wasmParserContents = fs.readFileSync(process.argv[2]).toString();
const fileContents = HEADER + wasmParserContents + FOOTER;

fs.writeFileSync(OUTPUT_FILE, fileContents);
