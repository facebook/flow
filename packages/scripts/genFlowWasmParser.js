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
  '../flow-parser-oxidized/dist/FlowParserWASM.js',
);

const HEADER = `/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

'use strict';
`;

// Add header and sign file before writing back to disk
const wasmParserContents = fs.readFileSync(process.argv[2]).toString();
const fileContents = HEADER + wasmParserContents;

fs.writeFileSync(OUTPUT_FILE, fileContents);
