/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import {join} from 'path';

import {getFlowErrorsWithWarnings} from './getFlowErrors';
import getAst from './getAst';

import {readFile, writeFile} from '../utils/async';
import {getNodeAtRange} from './getPathToLoc';

import type {Args} from './remove-commentsCommand';
import type {FlowLoc, FlowResult, FlowError, FlowMessage} from '../flowResult';
import type {Context} from './getContext';

async function getErrors(args: Args): Promise<Map<string, Array<FlowLoc>>> {
  const result: FlowResult = await getFlowErrorsWithWarnings(
    args.bin,
    args.errorCheckCommand,
    args.root,
    args.flowconfigName,
  );

  const errors = result.errors.filter(
    error =>
      (error.message[0].descr === 'Error suppressing comment' &&
        error.message[1].descr === 'Unused suppression') ||
      error.message[0].descr === 'Unused suppression comment.',
  );

  const errorsByFile = new Map();
  for (const error of errors) {
    const message = error.message[0];
    const loc = message.loc;
    if (loc) {
      const source = loc.source;
      if (source) {
        const file = join(args.root, source);
        const fileErrors: Array<FlowLoc> = errorsByFile.get(file) || [];
        fileErrors.push(loc);
        errorsByFile.set(file, fileErrors);
      }
    }
  }

  return errorsByFile;
}

async function removeUnusedErrorSuppressions(
  filename: string,
  errors: Array<FlowLoc>,
  flowBinPath: string,
): Promise<void> {
  const contentsString = await readFile(filename);
  const contents = await removeUnusedErrorSuppressionsFromText(
    Buffer.from(contentsString, 'utf8'),
    errors,
    flowBinPath,
  );
  await writeFile(filename, contents.toString('utf8'));
}

function bufferCharAt(buf: Buffer, pos: number): string {
  return buf.toString('utf8', pos, pos + 1);
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
  commentAST: Object | void,
  ast: Object,
) {
  const length = contents.length;

  const flowlintRegex = /^[ \t\n\r*]*flowlint(-line|-next-line)?( .*)?$/;
  const emptyFlowlintRegex = /^[ \t\n\r*]*flowlint(-line|-next-line)?[ \t\n\r*]*$/;
  if (commentAST && commentAST.value.match(flowlintRegex)) {
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

// Exported for testing
export async function removeUnusedErrorSuppressionsFromText(
  contents: Buffer,
  errors: Array<FlowLoc>,
  flowBinPath: string,
): Promise<Buffer> {
  // Sort in reverse order so that we remove comments later in the file first. Otherwise, the
  // removal of comments earlier in the file would outdate the locations for comments later in the
  // file.
  errors.sort((loc1, loc2) => loc2.start.offset - loc1.start.offset);

  const ast = await getAst(contents.toString('utf8'), flowBinPath);

  for (const error of errors) {
    const origStart = error.start.offset;
    const origEnd = error.end.offset;

    let commentAST;
    for (const comment of ast.comments) {
      const [commentStartOffset, commentEndOffset] = comment.range;
      if (origStart >= commentStartOffset && origEnd <= commentEndOffset) {
        commentAST = comment;
        break;
      }
    }

    // remove the comment and surrounding whitespace
    let [start, end] = expandComment(
      contents,
      origStart,
      origEnd,
      commentAST,
      ast,
    );
    contents = Buffer.concat([contents.slice(0, start), contents.slice(end)]);
  }
  return contents;
}

/* A flowtest is a file that ends in -flowtest.js or which is in a directory
 * named __flowtests__
 */
function isFlowtest(filename) {
  return (
    filename.match(/-flowtest\.js$/) ||
    filename.match(/[/\\]__flowtests__[/\\]/)
  );
}

export default async function(args: Args): Promise<void> {
  let ignoredFileCount = 0;
  let ignoredErrorCount = 0;
  let removedErrorCount = 0;
  const rawErrors = await getErrors(args);
  const errors = Array.from(rawErrors.entries())
    // Filter out flowtests
    .filter(([filename, errors]) => {
      if (!args.includeFlowtest && isFlowtest(filename)) {
        ignoredFileCount++;
        ignoredErrorCount += errors.length;
        return false;
      } else {
        removedErrorCount += errors.length;
      }
      return true;
    });
  await Promise.all(
    errors.map(([filename, errors]) =>
      removeUnusedErrorSuppressions(filename, errors, args.bin),
    ),
  );
  console.log(
    'Removed %d comments in %d files',
    removedErrorCount,
    errors.length,
  );
  if (ignoredFileCount > 0) {
    console.log(
      'Ignored %d comments in %d files due to -flowtest.js suffix or ' +
        '__flowtests__ directory. Run with `--include-flowtest` to also ' +
        'remove those files.',
      ignoredErrorCount,
      ignoredFileCount,
    );
  }
}
