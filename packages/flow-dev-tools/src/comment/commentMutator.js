/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import type {Context} from './getContext';
import type {FlowLoc} from '../flowResult';

const {NORMAL, JSX, JSX_FRAGMENT, TEMPLATE} = require('./getContext');
const {format} = require('util');

function bufferCharAt(buf: Buffer, pos: number): string {
  return buf.toString('utf8', pos, pos + 1);
}

const newlineRegex = /[\u2029\u2028\r\n]/;
const edible = /[\t ]/;

function findStartOfLine(contents: Buffer, startOffset: number): number {
  // if startOffset is already a newline, that's not the start of the line,
  // it's the end of the line. so start from the character before.
  let start = startOffset - 1;
  while (start >= 0 && !bufferCharAt(contents, start).match(newlineRegex)) {
    start--;
  }
  return start + 1;
}

function findEndOfLine(contents: Buffer, startOffset: number): number {
  let start = startOffset;
  while (
    start < contents.length &&
    !bufferCharAt(contents, start).match(newlineRegex)
  ) {
    start++;
  }
  return start;
}

function insertCommentToText(
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

function addCommentToText(
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
    const part2 = line.substr(start_col);

    const newCodeParts = [part1, ...formatComment(comments, padding, {})];

    // If the remainder of the line is empty we don't want to add a newline
    // since one already exists after the opening expression.
    if (part2.trim() !== '') {
      newCodeParts.push(padding + part2);
    }

    return Buffer.concat([
      contents.slice(0, start),
      Buffer.from(newCodeParts.join('\n')),
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
function formatComment(
  comments: Array<string>,
  line: string,
  args: {
    jsx?: boolean,
  },
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
      commentLines.push(format('%s */', padding));
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

module.exports = {
  addCommentToText,
};
