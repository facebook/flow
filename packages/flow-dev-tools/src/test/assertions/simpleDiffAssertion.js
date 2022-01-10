/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {format} = require('util');

const chalk = require('chalk');
const {diffLines, diffWords} = require('diff');

import type {
  AssertionLocation,
  ErrorAssertionResult,
  Suggestion,
} from './assertionTypes';

// Like str.trim(), but only trims blank lines at the beginning and end
function trim(str: string) {
  return str.replace(/^\s*\n/, '').replace(/\n\s*$/, '');
}

function getDiff(
  expected: string,
  actual: string,
  ignoreWhitespace: boolean = true,
): ?Array<string> {
  let isSame = true;
  let actualLine = '';
  let expectedLine = '';
  let messages: Array<string> = [];
  let parts = [];
  actual = trim(actual);
  expected = trim(expected);

  // Doing a word-by-word diff algorithm gets really slow for very large, very
  // different strings. While it's hard to estimate how different they are, it's
  // easy to tell when they're very large :)
  const numWords = actual.split(/\s+/).length + expected.split(/\s+/).length;
  if (numWords > 1000) {
    // diffLines trims every line when ignoreWhitespace is true. What I want is
    // for the diff algorithm to ignore whitespace but NOT trim whitespace. So
    // basically I need to look up the original line to get the correct
    // whitespace. Le sigh.
    const expectedLines = expected.split('\n');
    const actualLines = actual.split('\n');
    let aidx = 0,
      eidx = 0;
    for (const part of diffLines(actual, expected, {ignoreWhitespace})) {
      for (let value of part.value.split(/(\n)/)) {
        if (value != '') {
          if (value !== '\n') {
            if (part.added) {
              value = expectedLines[eidx];
            } else if (part.removed) {
              value = actualLines[aidx];
            } else {
              value = expectedLines[eidx] || actualLines[aidx];
            }
          } else {
            if (part.added) {
              eidx++;
            } else if (part.removed) {
              aidx++;
            } else {
              aidx++;
              eidx++;
            }
          }
          parts.push({
            added: part.added,
            removed: part.removed,
            value: value,
          });
        }
      }
    }
  } else {
    for (const part of diffWords(actual, expected)) {
      for (const value of part.value.split(/(\n)/)) {
        if (value != '') {
          const notWhitespace = !ignoreWhitespace || !value.match(/^\s+$/);
          parts.push({
            added: part.added && notWhitespace,
            removed: part.removed && notWhitespace,
            value,
          });
        }
      }
    }
  }

  const saveLine = () => {
    if (expectedLine == actualLine) {
      if (expectedLine == '') {
        return;
      }
      // Gotta make these line num with the +/- lines
      messages.push('  ' + expectedLine);
    } else {
      actualLine != '' && messages.push(chalk.green('+ ') + actualLine);
      expectedLine != '' && messages.push(chalk.red('- ') + expectedLine);
      isSame = false;
    }
    actualLine = expectedLine = '';
  };

  for (const part of parts) {
    if (part.value == '\n') {
      saveLine();
    } else {
      if (part.added) {
        expectedLine += chalk.red(part.value);
      } else if (part.removed) {
        actualLine += chalk.green(part.value);
      } else {
        expectedLine += chalk.grey(part.value);
        actualLine += chalk.grey(part.value);
      }
    }
  }
  saveLine();
  return isSame ? null : messages;
}

function simpleDiffAssertion(
  expected: string,
  actual: string,
  assertLoc: ?AssertionLocation,
  reason: ?string,
  diffSubject: string,
  suggestion: Suggestion,
  ignoreWhitespace: boolean = true,
): ErrorAssertionResult {
  const diffMessages = getDiff(expected, actual, ignoreWhitespace);
  if (diffMessages != null) {
    const locMessage =
      assertLoc == null
        ? []
        : [
            format(
              chalk.white('%s line %d col %d'),
              assertLoc.filename,
              assertLoc.line,
              assertLoc.column,
            ),
          ];
    const reasonMessage =
      reason == null
        ? []
        : [format(chalk.grey('Reason: ') + chalk.red('%s'), reason)];
    const keyMessage = [
      format(
        chalk.green('Actual %s (+)') +
          chalk.grey(" didn't match ") +
          chalk.red('expected %s (-)'),
        diffSubject,
        diffSubject,
      ),
    ];
    const messages = [].concat(
      locMessage,
      reasonMessage,
      keyMessage,
      diffMessages,
    );

    return {type: 'fail', messages, assertLoc, suggestion};
  }
  return {type: 'pass'};
}

module.exports = {
  default: simpleDiffAssertion,
};
