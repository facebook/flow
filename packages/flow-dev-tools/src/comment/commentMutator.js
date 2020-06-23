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
import {type Context, NORMAL, JSX, JSX_FRAGMENT, TEMPLATE} from './getContext';
import type {FlowLoc} from '../flowResult';
import {format} from 'util';

function bufferCharAt(buf: Buffer, pos: number): string {
  return buf.toString('utf8', pos, pos + 1);
}

const flowlintRegex = /^[ \t\n\r*]*flowlint(-line|-next-line)?\b/;
export function isLintSuppression(commentAST: {value: string}): boolean {
  return flowlintRegex.test(commentAST.value);
}

const newlineRegex = /[\u2029\u2028\r\n]/;
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

export function findStartOfLine(contents: Buffer, startOffset: number): number {
  let start = startOffset;
  while (start >= 0 && !bufferCharAt(contents, start).match(newlineRegex)) {
    start--;
  }
  return start + 1;
}

function findEndOfLine(contents: Buffer, startOffset): number {
  let start = startOffset;
  while (
    start < contents.length &&
    !bufferCharAt(contents, start).match(newlineRegex)
  ) {
    start++;
  }
  return start;
}

export function insertCommentToText(
  contents: Buffer,
  startOffset: number,
  comment: string,
): Buffer {
  return Buffer.concat([
    contents.slice(0, startOffset),
    Buffer.from(comment),
    contents.slice(startOffset),
  ]);
}

export function addCommentToText(
  contents: Buffer,
  loc: FlowLoc,
  inside: Context,
  comments: Array<string>,
  ast: any,
  startOfLine?: number,
): Buffer {
  let startOffset;
  let start;
  if (startOfLine == null) {
    startOffset = loc.start.offset;
    start = findStartOfLine(contents, startOffset);
  } else {
    start = startOfLine;
    startOffset = startOfLine;
  }

  const endOfLine = findEndOfLine(contents, startOffset);
  let line = contents.toString('utf8', start, endOfLine);
  const inJSX = inside === JSX_FRAGMENT || inside === JSX;
  if (inside === NORMAL) {
    return insertCommentToText(
      contents,
      start,
      formatComment(comments, line, {jsx: false}).join('\n') + '\n',
    );
  } else if (inJSX && ast.type === 'JSXElement') {
    return insertCommentToText(
      contents,
      start,
      formatComment(comments, line, {jsx: true}).join('\n') + '\n',
    );
  } else if (
    inside === TEMPLATE ||
    (inJSX && ast.type === 'JSXExpressionContainer')
  ) {
    /* Ok, so we have something like
     *
     * <jsx>
     *   {10 * 'hello'}
     * <jsx>
     *
     * We need to stick the comment inside the expression container.
     * So the above example turns into
     *
     * <jsx>
     *   {
     *     // Comment
     *     10 * 'hello'}
     * <jsx>
     *
     * Same thing if we have something like
     *
     * var str = `hello
     *   ${10 * 'hello'}
     * `;
     *
     * We need to stick the comment inside of the template element. So the
     * above example turns into
     *
     * var str = `hello
     *   ${
     *     // Comment
     *     10 * 'hello'}
     * `;
     */
    const start_col = inJSX ? ast.loc.start.column + 1 : ast.loc.start.column;
    const part1 = line.substr(0, start_col);
    const match = part1.match(/^ */);
    const padding = match ? match[0] + '  ' : '  ';
    const part2 = padding + line.substr(start_col);
    const newCode = []
      // $FlowFixMe unsealed object but should just be {||}
      .concat([part1], formatComment(comments, part2, {}), [part2])
      .join('\n');
    return Buffer.concat([
      contents.slice(0, start),
      Buffer.from(newCode),
      contents.slice(endOfLine),
    ]);
  } else if (inJSX && ast.type === 'JSXText') {
    /* Ignore the case where the error's loc starts after the last non-whitespace
     * character of the line. This can occur when an error's loc spans the
     * children of a JSX element. We cannot safely add a comment to the line
     * before the error's loc, as it may be contained within a JSXOpeningElement.
     *
     *     Loc
     *      |
     *      v
     * <jsx>
     *   JSXElement, JSXExpressionContainer, or JSXText here...
     * <jsx>
     */
    let firstNonWhitespaceCharacter = startOffset;
    let atEndOfLine = true;
    while (firstNonWhitespaceCharacter < contents.length) {
      if (
        bufferCharAt(contents, firstNonWhitespaceCharacter).match(newlineRegex)
      ) {
        break;
      } else if (
        bufferCharAt(contents, firstNonWhitespaceCharacter).match(edible)
      ) {
        firstNonWhitespaceCharacter++;
      } else {
        atEndOfLine = false;
        break;
      }
    }
    if (atEndOfLine) {
      return contents;
    }
    /*
     * Otherwise add an expression container above the text with our comment.
     *
     * <jsx>
     *   {// Comment}
     *   JSX Text Here
     * <jsx>
     */
    return insertCommentToText(
      contents,
      start,
      formatComment(comments, line, {jsx: true}).join('\n') + '\n',
    );
  }
  return contents;
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

/* Take up to `max` characters from str, trying to split at a space or dash or
 * something like that. */
function splitAtWord(str: string, max: number): [string, string] {
  let ret = '';
  let maybe = '';

  for (let i = 0; i < max; i++) {
    if (i === str.length) {
      ret += maybe;
      break;
    }
    maybe += str[i];
    if (str[i].match(/[- _\t]/)) {
      ret += maybe;
      maybe = '';
    }
  }

  // If there were no breaks then take it all
  if (ret === '') {
    ret = maybe;
  }

  return [ret, str.substr(ret.length)];
}

/* Figures out how to pad the comment and split it into multiple lines */
export function formatComment(
  comments: Array<string>,
  line: string,
  args: {|
    jsx?: boolean,
  |},
): Array<string> {
  const {jsx = false} = args;
  const match = line.match(/^ */);
  let padding = match ? match[0] : '';
  padding.length > 40 && (padding = '    ');

  if (jsx === false) {
    const singleLineComments = comments.map(comment =>
      format('%s// %s', padding, comment),
    );
    const allUnder80 = comments.reduce(
      (acc, comment) => acc && comment.length <= 80,
      true,
    );
    if (allUnder80) {
      return singleLineComments;
    }
  }

  const commentLines = [];
  const firstLinePrefix = format(!jsx ? '%s/* ' : '%s /* ', padding);
  for (let comment of comments) {
    let firstLineComment;
    [firstLineComment, comment] = splitAtWord(
      comment.trim(),
      80 - firstLinePrefix.length,
    );
    commentLines.push(firstLinePrefix + firstLineComment.trim());

    const prefix = format(!jsx ? '%s * ' : '%s  * ', padding);
    let commentLine;
    while (comment.length > 0) {
      [commentLine, comment] = splitAtWord(comment.trim(), 80 - prefix.length);
      commentLines.push(prefix + commentLine.trim());
    }
    if (commentLines[commentLines.length - 1].length < 76) {
      const last = commentLines.pop();
      commentLines.push(format('%s */', last));
    } else {
      commentLines.push(format('%s */%s', padding));
    }
  }
  if (jsx) {
    commentLines[0] = format('%s{%s', padding, commentLines[0].trim());
    commentLines[commentLines.length - 1] = format(
      '%s}',
      commentLines[commentLines.length - 1],
    );
  }
  return commentLines;
}
