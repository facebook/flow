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
  // Strip all \r characters.  This handles both \r\n (Windows line endings)
  // and standalone \r that can appear when Flow includes source lines from
  // files with \r\n endings — converting those to \n would create spurious
  // blank lines.
  return text.replace(/\r/g, '');
}

function normalizeWindowsEscapes(text: string): string {
  if (process.platform !== 'win32') {
    return text;
  }
  // Flow reads builtins files with \r\n line endings on Windows.
  // JSON output (e.g. type-at-pos documentation strings) contains literal
  // \r\n escape sequences where Linux has just \n.  Normalize these
  // BEFORE normalizeWindowsPaths runs, so that both sides compare equally
  // after backslash→forward-slash conversion.
  return text.replace(/\\r\\n/g, '\\n').replace(/\\r/g, '');
}

function normalizeWindowsPaths(text: string): string {
  if (process.platform !== 'win32') {
    return text;
  }
  // On Windows, Flow emits backslash path separators (e.g.
  // <BUILTINS>\core.js) but the .exp files use forward slashes.
  // Normalize both .exp and .out so that any backslashes (including those
  // in source-code snippets) are treated identically on both sides.
  return text.replace(/\\/g, '/');
}

function normalizeWindowsPointers(text: string): string {
  if (process.platform !== 'win32') {
    return text;
  }
  // Flow reads builtins with \r\n on Windows. The \r is counted as a
  // visible character, so pointer lines (v, ^, ~, -) get one extra
  // trailing dash.  Strip it from the ACTUAL output only so that
  // pointer widths match the .exp files (generated on Linux).
  return text.replace(/^(\s*[\^v~][\^v~-]*)-(\s*(?:\[\d+\])?\s*)$/gm, '$1$2');
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

  const exp = normalizeWindowsPaths(
    normalizeWindowsEscapes(
      normalizeLineEndings(substituteVersion(expRaw, version)),
    ),
  );
  const out = normalizeWindowsPointers(
    normalizeWindowsPaths(
      normalizeWindowsEscapes(normalizeLineEndings(outRaw)),
    ),
  );

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
    normalizeWindowsPointers(
      normalizeWindowsPaths(
        normalizeWindowsEscapes(normalizeLineEndings(outRaw)),
      ),
    ),
    version,
  );
  await fs.promises.writeFile(expFile, recorded);
}

module.exports = {
  diffOutput,
  recordOutput,
  normalizeLineEndings,
  normalizeWindowsEscapes,
  normalizeWindowsPaths,
  normalizeWindowsPointers,
  substituteVersion,
  substituteVersionForRecord,
};
