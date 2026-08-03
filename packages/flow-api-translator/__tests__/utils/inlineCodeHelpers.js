/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

/**
 * Align code based on the shortest whitespace offset
 */
export function trimToBeCode(toBeCode: string): string {
  const trimmedToBeCode = toBeCode.trim();
  const trimmedToBeCodeLines = trimmedToBeCode.split('\n');
  if (trimmedToBeCodeLines.length === 1) {
    return trimmedToBeCode + '\n';
  }

  let minSpaces = Infinity;
  const lines: Array<[number, string]> = [];
  for (let i = 1; i < trimmedToBeCodeLines.length; i++) {
    const line = trimmedToBeCodeLines[i];
    if (line === '') {
      lines.push([0, '']);
      continue;
    }
    const lineLeftTrimmed = line.trimLeft();
    const offset = line.length - lineLeftTrimmed.length;
    lines.push([offset, lineLeftTrimmed]);
    minSpaces = Math.min(minSpaces, offset);
  }
  if (minSpaces === 0) {
    return trimmedToBeCode + '\n';
  }

  let rebuiltStr = trimmedToBeCodeLines[0] + '\n';
  for (const [offset, line] of lines) {
    if (line === '') {
      rebuiltStr += '\n';
      continue;
    }
    rebuiltStr += ' '.repeat(offset - minSpaces) + line + '\n';
  }
  return rebuiltStr;
}
