/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import {spawn} from 'child_process';

/*::
type CheckContentsResult = {
  errors: Array<{
    message: Array<{
      descr: string,
      loc: {
        start: {line: number, column: number},
        end: {line: number, column: number},
      },
    }>,
    error_codes: ?Array<string>,
  }>,
};
*/

const MAX_UNRESOLVED_CHECK_CONTENTS = 10;
let unresolvedCheckContents = 0;
const checkContentsQueue /*: Array<() => unknown> */ = [];

async function checkContents(
  input /*: string */,
) /*: Promise<CheckContentsResult> */ {
  const flowProcess = spawn(process.env.FLOW_BIN_PATH || 'flow', [
    'check-contents',
    '--json',
    '--flowconfig-name',
    '.flowconfig.snippets',
  ]);
  flowProcess.stdin.end(input, 'utf8');
  let json = '';
  for await (const data of flowProcess.stdout) {
    json += data.toString();
  }
  return JSON.parse(json);
}

async function rateLimitedCheckContents(
  input /*: string */,
) /*: Promise<CheckContentsResult> */ {
  return new Promise(
    /*:: <CheckContentsResult> */ (resolve, reject) => {
      const run = async () => {
        unresolvedCheckContents++;
        try {
          resolve(await checkContents(input));
        } catch (error) {
          reject(error);
        } finally {
          unresolvedCheckContents--;
          const next = checkContentsQueue.shift();
          if (next != null) {
            next();
          }
        }
      };

      if (unresolvedCheckContents < MAX_UNRESOLVED_CHECK_CONTENTS) {
        run();
      } else {
        checkContentsQueue.push(run);
      }
    },
  );
}

export default async function getFlowMeta(
  code /*: string */,
  options /*: {[string]: boolean} */ = {},
) /*: Promise<string> */ {
  if (process.env.NO_INLINE_FLOW_ERRORS) {
    return JSON.stringify({errors: [], options});
  }
  const errors = (await rateLimitedCheckContents(code)).errors.map(
    ({message, error_codes}) => {
      const errorCode = (error_codes && error_codes[0]) || null;
      let fullDescription = message.map(({descr}) => descr).join(' ');

      // Strip error code tag from description text — Flow includes it inline
      // but we display it separately as a badge
      if (errorCode) {
        fullDescription = fullDescription
          .replace(`. [${errorCode}]`, '.')
          .replace(`[${errorCode}] `, '')
          .replace(`[${errorCode}]`, '');
      }

      return {
        messages: message.map(({loc, descr}) => ({
          startLine: loc.start.line,
          startColumn: loc.start.column,
          endLine: loc.end.line,
          endColumn: loc.end.column,
          description: descr,
        })),
        fullDescription,
        errorCode,
      };
    },
  );
  return JSON.stringify({errors, options});
}
