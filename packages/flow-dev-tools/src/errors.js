/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import path from 'path';
import {format} from 'util';
import {execManual} from './utils/async';

import type {FlowError, FlowLoc, FlowResult} from './flowResult';

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

export async function getUnusedSuppressionErrors(
  bin: string,
  errorCheckCommand: 'check' | 'status',
  root: string,
  flowconfigName: string,
): Promise<Array<FlowError>> {
  const result: FlowResult = await getFlowErrorsWithWarnings(
    bin,
    errorCheckCommand,
    root,
    flowconfigName,
  );

  return result.errors.filter(
    error =>
      (error.message[0].descr === 'Error suppressing comment' &&
        error.message[1].descr === 'Unused suppression') ||
      error.message[0].descr === 'Unused suppression comment.',
  );
}

export function collateLocs(
  errors: Array<FlowError>,
  root?: string,
): Map<string, Array<FlowLoc>> {
  const locsByFile = new Map();
  for (const error of errors) {
    const message = error.message[0];
    const loc = message.loc;
    if (loc) {
      const source = loc.source;
      if (source) {
        const file = root != null ? path.join(root, source) : source;
        const fileErrors: Array<FlowLoc> = locsByFile.get(file) || [];
        fileErrors.push(loc);
        locsByFile.set(file, fileErrors);
      }
    }
  }
  return locsByFile;
}
