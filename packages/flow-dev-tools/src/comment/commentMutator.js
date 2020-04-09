/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import {getNodeAtRange} from './getPathToLoc';

function bufferCharAt(buf: Buffer, pos: number): string {
  return buf.toString('utf8', pos, pos + 1);
}

const flowlintRegex = /^[ \t\n\r*]*flowlint(-line|-next-line)?\b/;
export function isLintSuppression(commentAST: {value: string}): boolean {
  return flowlintRegex.test(commentAST.value);
}

const edible = /[\t ]/;
/* This is the most confusing part of this command. A simple version of this
 * code would just remove exact characters of a comment. This might leave
 * extra whitespace and blank lines. So this code tries to expand the range
 * we remove to cover the following cases
 *
 * /* Comment with nothing before or after it * /
 * var foo; /* Comment with something before it * /
 * /* Comment with something after it * / var foo;
 * var foo; /* Comment with something before and after it * / var bar;
 *
 * The TL;DR is that we only want to expand the range and remove the newline
 * in the case where there is nothing before or after it
 */
function expandComment(
  contents: Buffer,
  startOffset: number,
  endOffset: number,
  commentAST: {value: string, range: [number, number]} | void,
  ast: Object,
) {
  const length = contents.length;

  const emptyFlowlintRegex = /^[ \t\n\r*]*flowlint(-line|-next-line)?[ \t\n\r*]*$/;
  if (commentAST && isLintSuppression(commentAST)) {
    // We're operating on a flowlint comment

    // All comments start with 2 chars
    const [commentStartOffset, commentEndOffset] = commentAST.range;
    const commentValueOffset = commentStartOffset + 2;

    if (
      commentAST.value[startOffset - commentValueOffset - 1].match(/[ \t\n\r]/)
    ) {
      // Remove the preceding whitespace when removing an element from the flowlint
      startOffset--;
    }

    const newCommentValue =
      commentAST.value.slice(0, startOffset - commentValueOffset) +
      commentAST.value.slice(endOffset - commentValueOffset);

    if (newCommentValue.match(emptyFlowlintRegex)) {
      startOffset = commentStartOffset;
      endOffset = commentEndOffset;
    } else {
      commentAST.range[1] -= endOffset - startOffset;
      commentAST.value = newCommentValue;
      // All we're doing is removing a piece of a flowlint comment. Return immediately
      return [startOffset, endOffset];
    }
  }

  let origBeforeStart = startOffset - 1;
  let origAfterEnd = endOffset;

  // Find the next interesting characters before and after the removed comment
  let beforeStart = origBeforeStart;
  let afterEnd = origAfterEnd;
  while (
    beforeStart >= 0 &&
    bufferCharAt(contents, beforeStart).match(edible)
  ) {
    beforeStart--;
  }
  while (afterEnd < length && bufferCharAt(contents, afterEnd).match(edible)) {
    afterEnd++;
  }

  if (
    beforeStart >= 0 &&
    afterEnd < length &&
    bufferCharAt(contents, beforeStart) === '{' &&
    bufferCharAt(contents, afterEnd) === '}'
  ) {
    // If this is JSX, then the curly braces start and stop a JSXExpressionContainer
    const node = getNodeAtRange([beforeStart, afterEnd + 1], ast);
    if (node && node.type == 'JSXExpressionContainer') {
      // Consume the curly braces
      beforeStart--;
      afterEnd++;

      // If we do reset our spacing, we'll at least eat the curly braces
      origBeforeStart = beforeStart;
      origAfterEnd = afterEnd;

      while (
        beforeStart >= 0 &&
        bufferCharAt(contents, beforeStart).match(edible)
      ) {
        beforeStart--;
      }
      while (
        afterEnd < length &&
        bufferCharAt(contents, afterEnd).match(edible)
      ) {
        afterEnd++;
      }
    }
  }

  if (beforeStart < 0 || bufferCharAt(contents, beforeStart) === '\n') {
    // There's nothing before the removed comment on this line
    if (afterEnd > length || bufferCharAt(contents, afterEnd) === '\n') {
      // The line is completely empty. Let's remove a newline from the start or
      // end of the line
      if (afterEnd < length) {
        afterEnd++;
      } else if (beforeStart >= 0) {
        beforeStart--;
      }
    } else {
      // There's something after the comment. We shouldn't remove
      // preceding whitespace thanks to indentation
      beforeStart = origBeforeStart;
    }
  } else if (afterEnd >= length || bufferCharAt(contents, afterEnd) === '\n') {
    // There's something preceding the comment but nothing afterwards. We can
    // just remove the rest of the line
  } else {
    beforeStart = origBeforeStart;
    afterEnd = origAfterEnd;
  }

  // The range should be [start, end) - that is includes start, excludes end
  return [beforeStart + 1, afterEnd];
}

export function removeUnusedErrorSuppressionFromText(
  contents: Buffer,
  startOffset: number,
  endOffset: number,
  commentAST: Object | void,
  ast: Object,
): Buffer {
  // remove the comment and surrounding whitespace
  let [start, end] = expandComment(
    contents,
    startOffset,
    endOffset,
    commentAST,
    ast,
  );

  return Buffer.concat([contents.slice(0, start), contents.slice(end)]);
}
