/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import {format} from 'util';
import {execManual} from './utils/async';

import type {FlowResult} from './flowResult';

async function getFlowErrorsImpl(
  bin,
  errorCheckCommand,
  root,
  withWarnings,
  flowconfigName,
) {
  const includeWarnings = withWarnings ? '--include-warnings' : '';
  const flowconfigNameFlag = '--flowconfig-name ' + flowconfigName;
  const cmd =
    errorCheckCommand === 'check'
      ? format(
          '%s check --strip-root --json %s %s %s',
          bin,
          includeWarnings,
          flowconfigNameFlag,
          root,
        )
      : format(
          '%s status --no-auto-start --strip-root --json %s %s %s',
          bin,
          includeWarnings,
          flowconfigNameFlag,
          root,
        );
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

export function getFlowErrorsWithWarnings(
  bin: string,
  errorCheckCommand: 'check' | 'status',
  root: string,
  flowconfigName: string,
): Promise<FlowResult> {
  return getFlowErrorsImpl(bin, errorCheckCommand, root, true, flowconfigName);
}

export async function getFlowErrors(
  bin: string,
  errorCheckCommand: 'check' | 'status',
  root: string,
  flowconfigName: string,
): Promise<FlowResult> {
  return getFlowErrorsImpl(bin, errorCheckCommand, root, false, flowconfigName);
}
