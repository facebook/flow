/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const fs = require('fs');
const {createTwoFilesPatch} = require('diff');

function normalizeLineEndings(text: string): string {
  return text.replace(/\r\n/g, '\n').replace(/\r/g, '\n');
}

function substituteVersion(text: string, version: string): string {
  return text.replace(/<VERSION>/g, version);
}

function substituteVersionForRecord(text: string, version: string): string {
  // Escape all regex-special characters in the version string, not just dots.
  // Semver can include + (build metadata) and - (pre-release) which are
  // regex quantifiers/special characters.
  const escaped = version.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  const re = new RegExp(escaped, 'g');
  return text.replace(re, '<VERSION>');
}

async function diffOutput(
  expFile: string,
  outFile: string,
  version: string,
): Promise<string> {
  const {existsAsync} = require('./checkExecFilePromise');
  if (!(await existsAsync(expFile))) {
    return 'Expected output file not found: ' + expFile + '\n';
  }
  if (!(await existsAsync(outFile))) {
    return 'Actual output file not found: ' + outFile + '\n';
  }
  const expRaw = await fs.promises.readFile(expFile, 'utf8');
  const outRaw = await fs.promises.readFile(outFile, 'utf8');

  const exp = normalizeLineEndings(substituteVersion(expRaw, version));
  const out = normalizeLineEndings(outRaw);

  if (exp === out) {
    return '';
  }

  // Generate unified diff matching `diff -u --strip-trailing-cr`
  const patch = createTwoFilesPatch(
    expFile,
    outFile,
    exp,
    out,
    '', // oldHeader
    '', // newHeader
    {context: 3},
  );

  return patch;
}

async function recordOutput(
  outFile: string,
  expFile: string,
  version: string,
): Promise<void> {
  const outRaw = await fs.promises.readFile(outFile, 'utf8');
  const recorded = substituteVersionForRecord(
    normalizeLineEndings(outRaw),
    version,
  );
  await fs.promises.writeFile(expFile, recorded);
}

module.exports = {
  diffOutput,
  recordOutput,
  normalizeLineEndings,
  substituteVersion,
  substituteVersionForRecord,
};
