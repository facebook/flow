/* @flow */

import {format} from 'util';

import colors from 'colors/safe';
import {diffWords} from 'diff';

import type {AssertionLocation, ErrorAssertionResult, Suggestion} from './assertionTypes';

function getDiff(expected: string, actual: string): ?Array<string> {
  let isSame = true;
  let actualLine = "";
  let expectedLine = "";
  let messages: Array<string> = [];
  let parts = [];
  for (const part of diffWords(actual.trim(), expected.trim())) {
    for (const value of part.value.split(/(\n)/)) {
      if (value != "") {
        parts.push({
          added: part.added,
          removed: part.removed,
          value,
        });
      }
    }
  }

  const saveLine = () => {
    if (expectedLine == actualLine) {
      if (expectedLine == "") {
        return;
      }
      // Gotta make these line num with the +/- lines
      messages.push("  " + expectedLine);
    } else {
      actualLine != "" && messages.push(colors.green("+ ") + actualLine);
      expectedLine != "" && messages.push(colors.red("- ") + expectedLine);
      isSame = false;
    }
    actualLine = expectedLine = "";
  }

  for (const part of parts) {
    if (part.value == "\n") {
      saveLine();
    } else {
      if (part.added) {
        expectedLine += colors.red(part.value);
      } else if (part.removed) {
        actualLine += colors.green(part.value);
      } else {
        expectedLine += colors.grey(part.value);
        actualLine += colors.grey(part.value);
      }
    }
  }
  saveLine();
  return isSame ? null : messages;
}

export default function(
  expected: string,
  actual: string,
  assertLoc: ?AssertionLocation,
  reason: ?string,
  diffSubject: string,
  suggestion: Suggestion,
): ErrorAssertionResult {

  const diffMessages = getDiff(expected, actual);
  if (diffMessages != null) {
    const locMessage = assertLoc == null ? [] : [format(
      colors.white("%s line %d col %d"),
      assertLoc.filename,
      assertLoc.line,
      assertLoc.column,
    )];
    const reasonMessage = reason == null ? [] : [format(
      colors.grey("Reason: ")+colors.red("%s"),
      reason,
    )];
    const keyMessage = [format(
      colors.green("Actual %s (+)") +
      colors.grey(" didn't match ") +
      colors.red("expected %s (-)"),
      diffSubject,
      diffSubject,
    )];
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
