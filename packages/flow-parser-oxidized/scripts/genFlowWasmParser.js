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

const OUTPUT_FILE = path.resolve(__dirname, '../dist/FlowParserWASM.js');

const HEADER = `/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

'use strict';
`;

const inputPath = process.argv[2];
if (inputPath == null) {
  console.error(
    'Usage: node genFlowWasmParser.js <path-to-wasm-js>\n' +
      '  Prepends the MIT license header to the emcc-built JS and writes it to dist/.',
  );
  process.exit(1);
}

const wasmParserContents = fs.readFileSync(inputPath).toString();
fs.writeFileSync(OUTPUT_FILE, HEADER + wasmParserContents);
