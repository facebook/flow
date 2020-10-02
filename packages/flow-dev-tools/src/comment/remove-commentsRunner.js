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

import {collateLocs, getUnusedSuppressionErrors} from '../errors';
import getAst from './getAst';

import {readFile, writeFile} from '../utils/async';
import {removeUnusedErrorSuppressionFromText} from './commentMutator';

import type {Args} from './remove-commentsCommand';
import type {FlowLoc, FlowResult, FlowError, FlowMessage} from '../flowResult';
import type {Context} from './getContext';

async function getErrors(args: Args): Promise<Map<string, Array<FlowLoc>>> {
  const errors: Array<FlowError> = await getUnusedSuppressionErrors(
    args.bin,
    args.errorCheckCommand,
    args.root,
    args.flowconfigName,
  );
  return collateLocs(errors);
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

    contents = removeUnusedErrorSuppressionFromText(
      contents,
      origStart,
      origEnd,
      commentAST,
      ast,
    );
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
