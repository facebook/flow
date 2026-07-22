/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {readFile, writeFile} = require('fs').promises;
const getPathToLoc = require('./getPathToLoc').default;
const {
  getFlowErrors,
  filterErrors,
  mainSourceLocOfError,
} = require('../errors');
const getContext = require('./getContext').default;
const getAst = require('./getAst').default;

import type {PathNode} from './getPathToLoc';
import type {Args} from './add-commentsCommand';
import type {FlowLoc, FlowError} from '../flowResult';
const {addCommentToText} = require('./commentMutator');

export type Suppression = {
  loc: FlowLoc,
  error_codes: Array<string>,
};

async function runner(args: Args): Promise<void> {
  let flowResult = await getFlowErrors(
    args.bin,
    args.errorCheckCommand,
    args.root,
    args.flowconfigName,
  );

  if (flowResult.passed) {
    console.log('No errors found. Nothing to do. Exiting');
    return;
  }

  const errors = filterErrors(flowResult.errors);

  await addComments(args, errors);
  process.exit(0);
}

async function addComments(args: Args, errors: Array<FlowError>) {
  let filenameToLineToLocsMap: Map<
    string,
    Map<number, Suppression>,
  > = new Map();
  // Filter out errors without a main location
  let errorCount = 0;
  for (const error of errors) {
    const loc = mainSourceLocOfError(error);
    const error_codes = error.error_codes;
    if (loc != null && loc.source != null) {
      const source = loc.source;
      const lineToLocsMap = filenameToLineToLocsMap.get(source) || new Map();
      function joinSuppression(
        prevValue: ?Suppression,
        newValue: Suppression,
      ): Suppression {
        if (!prevValue) {
          return newValue;
        }
        return {
          loc: newValue.loc,
          error_codes: [...newValue.error_codes, ...prevValue.error_codes],
        };
      }
      const prevValue: ?Suppression = lineToLocsMap.get(loc.start.line);
      const value = joinSuppression(prevValue, {
        loc,
        error_codes,
      });
      lineToLocsMap.set(loc.start.line, value);
      filenameToLineToLocsMap.set(source, lineToLocsMap);
      errorCount++;
    }
  }

  const promises = [];
  for (const [source, lineToLocsMap] of filenameToLineToLocsMap.entries()) {
    promises.push(
      addCommentsToSource(args, source, Array.from(lineToLocsMap.values())),
    );
  }
  const counts = await Promise.all(promises);
  const commentCount = counts.reduce((c1, c2) => c1 + c2, 0);
  console.log(
    'Added %d comments to suppress %d errors',
    commentCount,
    errorCount,
  );
}

/* A single file needs 1 or more comments added. Start at the bottom of the
 * file, and add comments going up. Then write the changes */
async function addCommentsToSource(
  args: Args,
  source: string,
  locs: Array<Suppression>,
): Promise<number> {
  const codeString = await readFile(source, 'utf8');

  const [code, commentCount] = await addCommentsToCode(
    args.comment,
    args.error_code,
    codeString,
    locs,
    args.bin,
  );
  await writeFile(source, code);
  return commentCount;
}

function addCommentsToCodeInternal(
  comments: Array<string>,
  code: string,
  loc: FlowLoc,
  path: Array<PathNode>,
) {
  const [inside, ast] = getContext(loc, path);
  return addCommentToText(
    Buffer.from(code),
    loc,
    inside,
    comments,
    ast,
  ).toString();
}

async function addCommentsToCode(
  comment: ?string,
  error_code: ?string,
  code: string,
  locs: Array<Suppression>,
  flowBinPath: string,
): Promise<
  [string, number],
> /* [resulting code, number of comments inserted] */ {
  locs.sort((l1, l2) => l2.loc.start.line - l1.loc.start.line);

  const ast = await getAst(code, flowBinPath);

  let commentCount = 0;
  for (let {loc, error_codes} of locs) {
    if (error_code != null) {
      error_codes = error_codes.filter(c => c === error_code);
    }
    if (error_codes.length === 0) {
      continue;
    }

    const path = getPathToLoc(loc, ast);

    if (path != null) {
      let c = comment || '';
      const comments = [...new Set(error_codes)].map(
        error_code => `$FlowFixMe[${error_code}]${c ? ` ${c}` : ''}`,
      );

      // The order doesn't matter for suppression comments. For implementation reasons
      // we had comments in reverse order. This is now no longer the case but we are
      // preserving this behavour to make testing against existing suppression
      // locations easier.
      comments.reverse();

      code = addCommentsToCodeInternal(comments, code, loc, path);
      commentCount += error_codes.length;
    }
  }
  return [code, commentCount];
}

module.exports = {
  addCommentsToCode,
  default: runner,
};
