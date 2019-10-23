/*
 * @flow
 * @format
 */

import {join} from 'path';

import {getFlowErrorsWithWarnings} from './getFlowErrors';
import getAst from './getAst';
import getPathToLoc from './getPathToLoc';
import getContext, {NORMAL, JSX, TEMPLATE} from './getContext';

import {readFile, writeFile} from '../utils/async';

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
  let contents = await readFile(filename);
  contents = await removeUnusedErrorSuppressionsFromText(
    contents,
    errors,
    flowBinPath,
  );
  await writeFile(filename, contents);
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
  contents: string,
  startOffset: number,
  endOffset: number,
  context: Context,
) {
  const length = contents.length;

  // ranges are [start, end). we're interested in the values of the characters
  // within the range, so for origEnd, we subtract 1 from the end offset to
  // make it inclusive.
  let origStart = startOffset;
  let origEnd = endOffset - 1;

  if (
    context === JSX &&
    contents[origStart - 1] === '{' &&
    contents[origEnd + 1] === '}'
  ) {
    origStart--;
    origEnd++;
  }

  let start = origStart;
  let end = origEnd;

  while (start > 0) {
    // Eat whitespace towards the start of the line
    if (contents[start - 1].match(edible)) {
      start--;
    } else if (contents[start - 1] === '\n') {
      // If we make it to the beginning of the line, awesome! Let's try and
      // expand the end too!
      while (end < length - 1) {
        // Eat whitespace towards the end of the line
        if (contents[end + 1].match(edible)) {
          end++;
        } else if (contents[end + 1] === '\n') {
          // If we make it to both the beginning and the end of the line,
          // then we can totally remove a newline!
          end++;
          break;
        } else {
          // Otherwise we can't, undo the expansion
          start = origStart;
          end = origEnd;
          break;
        }
      }
      break;
    } else {
      // If we hit something else, then undo the start expansion
      start = origStart;
      break;
    }
  }

  // as above, ranges are [X, Y) but in this function, `end` is inclusive. so
  // add 1 to make it exclusive again.
  return [start, end + 1];
}
// Exported for testing
export async function removeUnusedErrorSuppressionsFromText(
  contents: string,
  errors: Array<FlowLoc>,
  flowBinPath: string,
): Promise<string> {
  // Sort in reverse order so that we remove comments later in the file first. Otherwise, the
  // removal of comments earlier in the file would outdate the locations for comments later in the
  // file.
  errors.sort((loc1, loc2) => loc2.start.offset - loc1.start.offset);

  const ast = await getAst(contents, flowBinPath);

  for (const error of errors) {
    const path = getPathToLoc(error, ast);
    let context: Context;
    if (path == null) {
      context = NORMAL;
    } else {
      [context] = getContext(error, path);
    }

    const origStart = error.start.offset;
    const origEnd = error.end.offset;

    // remove the comment and surrounding whitespace
    let [start, end] = expandComment(contents, origStart, origEnd, context);
    contents = contents.slice(0, start) + contents.slice(end);
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
