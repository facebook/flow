/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

/**
 * Codemod to use specific Flow suppressions.
 * Usage: flow --include-warnings --show-all-errors --json | node add-specific-error-code.js
 */

const fs = require('fs');

const {errors} = JSON.parse(fs.readFileSync(0, 'utf-8'));

const DESC_MARKER =
  'Suppression is missing a code. Please update this suppression to use an error code: `$FlowFixMe[';

const processedLines = new Set();
for (const {kind, message} of errors.reverse()) {
  try {
    if (kind !== 'infer') {
      continue;
    }
    const firstMessage = message[0];
    if (firstMessage == null) {
      continue;
    }
    const {descr, path, line} = firstMessage;
    if (!descr.startsWith(DESC_MARKER)) {
      continue;
    }
    const expectedErrorCode = descr.substring(
      DESC_MARKER.length,
      descr.length - 2,
    );
    const alreadyProcessed = processedLines.has(`${path}:${line}`);
    const oldContent = fs.readFileSync(path).toString();
    let newContent;
    if (alreadyProcessed) {
      const oldLines = oldContent.split('\n');
      newContent = [
        ...oldLines.slice(0, line - 1),
        `// \$FlowFixMe[${expectedErrorCode}]`,
        ...oldLines.slice(line - 1),
      ].join('\n');
    } else {
      processedLines.add(`${path}:${line}`);
      newContent = oldContent
        .split('\n')
        .map((lineStr, lineNo) => {
          if (lineNo + 1 !== line) {
            return lineStr;
          }
          return lineStr
            .replace('$FlowFixMe', `\$FlowFixMe[${expectedErrorCode}]`)
            .replace(
              '$FlowExpectedError',
              `\$FlowExpectedError[${expectedErrorCode}] `,
            )
            .replace('$FlowIgnore', `\$FlowIgnore[${expectedErrorCode}]`)
            .replace('$FlowIssue', `\$FlowIssue[${expectedErrorCode}]`);
        })
        .join('\n');
    }
    if (oldContent === newContent) {
      continue;
    }
    fs.writeFileSync(path, newContent);
    console.error('Migrated', path);
  } catch {
    continue;
  }
}
