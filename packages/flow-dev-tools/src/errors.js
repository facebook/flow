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
          '%s check --json %s %s %s',
          bin,
          includeWarnings,
          flowconfigNameFlag,
          root,
        )
      : format(
          '%s status --no-auto-start --json %s %s %s',
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

export function isUnusedSuppression(error: FlowError): boolean {
  return (
    (error.message[0].descr === 'Error suppressing comment' &&
      error.message[1].descr === 'Unused suppression') ||
    error.message[0].descr === 'Unused suppression comment.'
  );
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

  return result.errors.filter(isUnusedSuppression);
}

export function collateLocs(
  errors: Array<FlowError>,
): Map<string, Array<FlowLoc>> {
  const errorsByFile = collateErrors(errors);
  const locsByFile = new Map();
  for (const [file, errors] of errorsByFile) {
    locsByFile.set(
      file,
      errors.reduce((acc, error) => {
        const loc = error.message[0].loc;
        if (loc != null) {
          acc.push(loc);
        }
        return acc;
      }, []),
    );
  }
  return locsByFile;
}

export function mainSourceLocOfError(error: FlowError): ?FlowLoc {
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
export function filterErrors(errors: Array<FlowError>): Array<FlowError> {
  return errors.filter(e => mainSourceLocOfError(e) != null);
}

export function collateErrors(
  errors: Array<FlowError>,
): Map<string, Array<FlowError>> {
  const errorsByFile = new Map();
  for (const error of errors) {
    const message = error.message[0];
    const loc = message.loc;
    if (loc) {
      const source = loc.source;
      if (source) {
        const fileErrors: Array<FlowError> = errorsByFile.get(source) || [];
        fileErrors.push(error);
        errorsByFile.set(source, fileErrors);
      }
    }
  }
  return errorsByFile;
}
