/* @flow */

import {join} from 'path';

import {getFlowErrorsWithWarnings} from './getFlowErrors';

import {readFile, writeFile} from '../async';

import type {Args} from './remove-commentsCommand';
import type {FlowLoc, FlowResult, FlowError, FlowMessage} from '../flowResult';

type Loc = {
  start: number,
  end: number,
};

async function getErrors(args: Args): Promise<Map<string, Array<Loc>>> {
  const result = await getFlowErrorsWithWarnings(
    args.bin,
    args.errorCheckCommand,
    args.root,
  );

  const errors = result.errors.filter(error =>
    error.message[0].descr === "Error suppressing comment" &&
    error.message[1].descr === "Unused suppression"
  );

  const errorsByFile = new Map();;
  for (const error of errors) {
    if (error.message[0].loc && error.message[0].loc.source) {
      const start = error.message[0].loc.start.offset;
      const end = error.message[0].loc.end.offset;
      const file = join(args.root, error.message[0].loc.source);
      const fileErrors = errorsByFile.get(file) || [];
      fileErrors.push({start, end});
      errorsByFile.set(file, fileErrors);
    }
  }

  // Group errors by file and sort by reverse location in the file
  for (const file of errorsByFile.keys()) {
    const fileErrors = errorsByFile.get(file);
    fileErrors && fileErrors.sort((e1, e2) => e2.start - e1.start);
  }
  return errorsByFile;
}

async function removeUnusedErrorSuppressions(
  [filename, errors]: [string, Array<Loc>],
): Promise<void> {
  let contents = await readFile(filename);

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
  const edible = /[\t ]/;
  for (let {start: origStart, end: origEnd} of errors) {
    const length = contents.length;
    origEnd--;
    let start = origStart;
    let end = origEnd;
    while (start > 0) {
      // Eat whitespace towards the start of the line
      if (contents[start-1].match(edible)) {
        start--;
      } else if (contents[start-1] === "\n") {
        // If we make it to the beginning of the line, awesome! Let's try and
        // expand the end too!
        while (end < length - 1) {
          // Eat whitespace towards the end of the line
          if (contents[end+1].match(edible)) {
            end++;
          } else if (contents[end+1] === "\n") {
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
    contents = contents.slice(0, start) + contents.slice(end+1);
  }
  await writeFile(filename, contents);
}

/* A flowtest is a file that ends in -flowtest.js or which is in a directory
 * named __flowtests__
 */
function isFlowtest(filename) {
  return filename.match(/-flowtest\.js$/) ||
         filename.match(/[/\\]__flowtests__[/\\]/);
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
  await Promise.all(errors.map(removeUnusedErrorSuppressions));
  console.log(
    "Removed %d comments in %d files",
    removedErrorCount,
    errors.length,
  );
  if (ignoredFileCount > 0) {
    console.log(
      "Ignored %d comments in %d files due to -flowtest.js suffix or " +
        "__flowtests__ directory. Run with `--include-flowtest` to also " +
        "remove those files.",
      ignoredErrorCount,
      ignoredFileCount
    );
  }
}
