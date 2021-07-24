/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import util from 'util';

import {join} from 'path';

import getFlowFiles from './getFlowFiles';
import {
  collateErrors,
  getFlowErrorsWithWarnings,
  isUnusedSuppression,
  filterErrors,
} from '../errors';
import getAst from '../comment/getAst';

import {readFile, writeFile} from '../utils/async';
import {
  removeUnusedErrorSuppressionFromText,
  isLintSuppression,
  addCommentToText,
  findStartOfLine,
} from '../comment/commentMutator';

import type {Args, RootName, RootPath} from './update-suppressionsCommand';
import type {FlowLoc, FlowResult, FlowError, FlowMessage} from '../flowResult';
import getContext from '../comment/getContext';
import getPathToLoc from '../comment/getPathToLoc';

type LocKey = string;

type ErrorsForLine = {|
  // Flow only reports one unused suppression per line
  unusedSuppressions: ?{roots: Set<RootName>, loc: FlowLoc, bins: Set<string>},
  errorCodes: Set<string>,
  loc: FlowLoc,
|};

function locKey(loc: FlowLoc): LocKey {
  return `${loc.start.offset}-${loc.end.offset}`;
}

async function getFiles(args: Args): Promise<Map<string, Set<RootName>>> {
  const {bin, roots, rootNames, flowconfigName} = args;
  const rootsByFile = new Map();
  for (let i = 0; i < roots.length; i++) {
    const root = roots[i];
    const rootName = rootNames[i];
    if (rootName == null) {
      throw new Error(`name for ${root} not set`);
    }
    const files = await getFlowFiles(bin, root, flowconfigName);
    for (let file of files) {
      const result = rootsByFile.get(file) || new Set();
      result.add(rootName);
      rootsByFile.set(file, result);
    }
  }
  return rootsByFile;
}

async function getErrorsForAllRootsForBinary(
  bin: string,
  roots: Array<RootPath>,
  rootNames: Array<RootName>,
  errorCheckCommand: 'check' | 'status',
  flowconfigName: string,
  only: ?('add' | 'remove'),
  errorsByFile: Map<string, Map<number, ErrorsForLine>>,
): Promise<Map<string, Map<number, ErrorsForLine>>> {
  for (let i = 0; i < roots.length; i++) {
    const root = roots[i];
    const rootName = rootNames[i];
    let errors: Array<FlowError> = (
      await getFlowErrorsWithWarnings(
        bin,
        errorCheckCommand,
        root,
        flowconfigName,
      )
    ).errors;
    for (const [file, errorsInFile] of collateErrors(errors)) {
      const errorsByLine: Map<number, ErrorsForLine> =
        errorsByFile.get(file) || new Map();
      const errorsInFileWithMainSourceLocs = filterErrors(errorsInFile);
      for (const error of errorsInFileWithMainSourceLocs) {
        if (error.level === 'warning' && !isUnusedSuppression(error)) {
          // Skip regular warnings
          continue;
        }
        // Only errors with locations can be in here
        if (error.message[0].loc === null) {
          throw new Error('Found an error without a location');
        } else {
          // $FlowFixMe loc is non-null from check above
          const loc: FlowLoc = error.message[0].loc;
          const dataForLine: ErrorsForLine = errorsByLine.get(
            loc.start.line,
          ) || {
            unusedSuppressions: null,
            errorCodes: new Set(),
            loc,
          };
          if (isUnusedSuppression(error) && only !== 'add') {
            const unusedSuppressions = dataForLine.unusedSuppressions || {
              roots: new Set(),
              bins: new Set(),
              loc,
            };
            unusedSuppressions.roots.add(rootName);
            unusedSuppressions.bins.add(bin);
            dataForLine.unusedSuppressions = unusedSuppressions;
          } else if (only !== 'remove') {
            for (const code of error.error_codes) {
              dataForLine.errorCodes.add(code);
            }
          }
          errorsByLine.set(loc.start.line, dataForLine);
        }
      }
      errorsByFile.set(file, errorsByLine);
    }
  }
  return errorsByFile;
}

async function getErrorsForAllRoots(
  args: Args,
): Promise<Map<string, Map<number, ErrorsForLine>>> {
  const {
    bin,
    diffBin,
    errorCheckCommand,
    roots,
    rootNames,
    flowconfigName,
    only,
  } = args;
  let errorsByFile: Map<string, Map<number, ErrorsForLine>> = new Map();
  errorsByFile = await getErrorsForAllRootsForBinary(
    bin,
    roots,
    rootNames,
    errorCheckCommand,
    flowconfigName,
    only,
    errorsByFile,
  );
  if (diffBin != null) {
    errorsByFile = await getErrorsForAllRootsForBinary(
      diffBin,
      roots,
      rootNames,
      errorCheckCommand,
      flowconfigName,
      only,
      errorsByFile,
    );
  }
  return errorsByFile;
}

function getSites(text: string): Set<string> {
  const match = /\bsite=([a-z,_]+)\)/.exec(text);
  return new Set((match && match[1].split(',')) || []);
}

// exported for tests
export function replaceSites(
  text: string,
  sites: $ReadOnlyArray<string>,
): string {
  const remainingSitesStr = [...sites].sort().join(',');

  if (/\bsite=([a-z,_]+)\)/.test(text)) {
    // has existing sites, replace them
    return text.replace(/\bsite=([a-z,_]+)\)/, `site=${remainingSitesStr})`);
  } else {
    // no existing sites, try to insert within existing parens
    let newText = text.replace(
      /^( *[^ ]+)\(([^)]+)\)/,
      `$1($2 site=${remainingSitesStr})`,
    );

    // if there were no existing parens, add some
    if (newText == text) {
      newText = text.replace(/^( *[^ ]+)/, `$1(site=${remainingSitesStr})`);
    }

    return newText;
  }
}

/**
 * Updates the `(site=...)` of a suppression comment, or removes the whole
 * comment if it's unused everywhere.
 */
function updateErrorSuppression(
  contents: Buffer,
  startOffset: number,
  endOffset: number,
  commentAST: Object | void,
  ast: Object,
  knownRoots: Set<RootName>,
  unusedRoots: Set<RootName>,
): Buffer {
  let commentStartOffset;
  let commentEndOffset;
  if (commentAST) {
    [commentStartOffset, commentEndOffset] = commentAST.range;
  } else {
    commentStartOffset = startOffset;
    commentEndOffset = endOffset;
  }
  let innerOffset = commentStartOffset + 2; // `/*` and `//` are both 2 chars
  let text = contents.slice(innerOffset, endOffset).toString('utf8');

  const existingRoots = getSites(text); // may include unknown roots
  const roots = new Set([...existingRoots, ...knownRoots]);
  unusedRoots.forEach(root => roots.delete(root));

  if (roots.size === 0) {
    // no roots need the suppression, so remove it
    return removeUnusedErrorSuppressionFromText(
      contents,
      startOffset,
      endOffset,
      commentAST,
      ast,
    );
  } else {
    if (commentAST && isLintSuppression(commentAST)) {
      // lints can't be suppressed per site, so we have to convert it to a
      // $FlowFixMe(site=<roots>)
      const sites = [...roots].sort().join(',');
      text = ` $FlowFixMe(site=${sites}) --${text}`;
    } else {
      // only sites in `roots` need the suppression, so add/update site=
      text = replaceSites(text, [...roots]);
    }

    return Buffer.concat([
      contents.slice(0, innerOffset),
      Buffer.from(text),
      contents.slice(endOffset),
    ]);
  }
}

async function updateSuppressions(
  filename: string,
  allRoots: Set<RootName>,
  numBins: number,
  errors: Map<number, ErrorsForLine>,
  comment: string,
  flowBinPath: string,
): Promise<void> {
  const contentsString = await readFile(filename);
  const contents = await updateSuppressionsInText(
    Buffer.from(contentsString, 'utf8'),
    allRoots,
    numBins,
    errors,
    comment,
    flowBinPath,
  );
  await writeFile(filename, contents.toString('utf8'));
}

// Exported for testing
export async function updateSuppressionsInText(
  contents: Buffer,
  allRoots: Set<RootName>,
  numBins: number,
  errorsByLine: Map<number, ErrorsForLine>,
  comment: string,
  flowBinPath: string,
): Promise<Buffer> {
  const errors = Array.from(errorsByLine);
  // Sort in reverse order so that we remove comments later in the file first. Otherwise, the
  // removal of comments earlier in the file would outdate the locations for comments later in the
  // file.
  errors.sort(([line1, _errs1], [line2, _errs2]) => line2 - line1);

  const ast = await getAst(contents.toString('utf8'), flowBinPath);

  for (const [
    _line,
    {loc: errorLoc, errorCodes, unusedSuppressions},
  ] of errors) {
    // For each line we:
    // 1. Get the start of the line. All changes will go _AFTER_ this line.
    // 2. Remove the unused supressions from the line
    // 3. Add the error suppressions at the original start of the line.
    // This ensures that we do not disrupt any offsets for errors coming before this one in the file.
    const beginningOfLine = findStartOfLine(contents, errorLoc.start.offset);

    /* Check if suppression is unused in all binaries */
    if (
      unusedSuppressions != null &&
      unusedSuppressions.bins.size === numBins
    ) {
      // Need to remove a suppression
      const loc = unusedSuppressions.loc;
      const origStart = loc.start.offset;
      const origEnd = loc.end.offset;
      let commentAST;
      for (const comment of ast.comments) {
        const [commentStartOffset, commentEndOffset] = comment.range;
        if (origStart >= commentStartOffset && origEnd <= commentEndOffset) {
          commentAST = comment;
          break;
        }
      }

      contents = updateErrorSuppression(
        contents,
        origStart,
        origEnd,
        commentAST,
        ast,
        allRoots,
        unusedSuppressions.roots,
      );
    }
    if (errorCodes.size > 0) {
      /* Need to add a suppression.
       * First of all, we want to know if we can add a comment to the line before
       * the error. So we need to see if we're in a JSX children block or inside a
       * template string when we reach the line with the error */
      const path = getPathToLoc(errorLoc, ast);
      if (path != null) {
        const [inside, ast] = getContext(errorLoc, path);
        const suppressionComments = Array.from(errorCodes).map(code =>
          comment !== ''
            ? `$FlowFixMe[${code}] ${comment}`
            : `$FlowFixMe[${code}]`,
        );
        contents = addCommentToText(
          contents,
          errorLoc,
          inside,
          suppressionComments,
          ast,
          beginningOfLine,
        );
      }
    }
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
  let addedSuppressionCount = 0;
  let filesWithUnusedSuppressionsCount = 0;
  let filesWithNeededSuppressionsCount = 0;
  const rootsByFile: Map<string, Set<RootName>> = await getFiles(args);
  const rawErrors: Map<
    string,
    Map<number, ErrorsForLine>,
  > = await getErrorsForAllRoots(args);
  const errors = Array.from(rawErrors.entries())
    // Filter out flowtests and generate stats
    .filter(([filename, errors]) => {
      if (!args.includeFlowtest && isFlowtest(filename)) {
        ignoredFileCount++;
        ignoredErrorCount += errors.size;
        return false;
      } else {
        let fileHasUnusedSuppression = false;
        let fileNeedsSuppression = false;
        for (const [
          _loc,
          {unusedSuppressions, errorCodes},
        ] of errors.entries()) {
          if (errorCodes.size > 0) {
            addedSuppressionCount += errorCodes.size;
            fileNeedsSuppression = true;
          }
          if (unusedSuppressions != null) {
            removedErrorCount++;
            fileHasUnusedSuppression = true;
          }
        }
        if (fileHasUnusedSuppression) {
          filesWithUnusedSuppressionsCount++;
        }
        if (fileNeedsSuppression) {
          filesWithNeededSuppressionsCount++;
        }
      }
      return true;
    });
  const errorBatches = [];
  while (errors.length) {
    errorBatches.push(errors.splice(0, 500));
  }
  for (const errors of errorBatches) {
    await Promise.all(
      errors.map(([filename, errors]) => {
        const allRoots = rootsByFile.get(filename);
        if (allRoots == null) {
          throw new Error(`${filename} not in any site???`);
        }
        return updateSuppressions(
          filename,
          allRoots,
          args.diffBin ? 2 : 1,
          errors,
          args.comment,
          args.bin,
        );
      }),
    );
  }
  console.log(
    'Removed %d comments in %d files',
    removedErrorCount,
    errors.length,
  );
  console.log(
    'Added %d comments in %d files',
    addedSuppressionCount,
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
