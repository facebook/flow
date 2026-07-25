/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const path = require('path');
const {format} = require('util');
const {execManual} = require('./utils/async');

import type {FlowError, FlowLoc, FlowResult} from './flowResult';

export type ErrorCheckCommand =
  | 'full-check'
  | 'check' // alias for full-check
  | 'status';

async function getFlowErrorsImpl(
  bin: string,
  errorCheckCommand: ErrorCheckCommand,
  root: string,
  flowconfigName: string,
) {
  const flowconfigNameFlag = '--flowconfig-name ' + flowconfigName;
  const cmd = match (errorCheckCommand) {
    'full-check' | 'check' =>
      format('%s full-check --json %s %s', bin, flowconfigNameFlag, root),
    'status' =>
      format(
        '%s status --no-auto-start --json %s %s',
        bin,
        flowconfigNameFlag,
        root,
      ),
  };
  const [err, stdout, stderr] = await execManual(cmd, {
    cwd: root,
    maxBuffer: Infinity,
  });

  // 0 - no errors
  // 2 - Some errors
  if (err == null || err.code === 2) {
    return JSON.parse(stdout.toString());
  }

  throw new Error(format('Flow check failed!', err, stdout, stderr));
}

async function getFlowErrors(
  bin: string,
  errorCheckCommand: ErrorCheckCommand,
  root: string,
  flowconfigName: string,
): Promise<FlowResult> {
  return getFlowErrorsImpl(bin, errorCheckCommand, root, flowconfigName);
}

function mainSourceLocOfError(error: FlowError): ?FlowLoc {
  const {operation, message} = error;
  for (const msg of [operation, ...message]) {
    if (msg && msg.loc && msg.loc.type === 'SourceFile') {
      return msg.loc;
    }
  }
  return null;
}

/**
 * Filter out errors without a main location or a source file
 */
function filterErrors(errors: Array<FlowError>): Array<FlowError> {
  return errors.filter(e => mainSourceLocOfError(e) != null);
}

module.exports = {
  getFlowErrors,
  mainSourceLocOfError,
  filterErrors,
};
