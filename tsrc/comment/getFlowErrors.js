/* @flow */

import {format} from 'util';
import {execManual} from '../utils/async';

import type {FlowResult} from '../flowResult';

async function getFlowErrors(bin, errorCheckCommand, root, withWarnings) {
  const includeWarnings = withWarnings ? '--include-warnings' : '';
  const cmd = errorCheckCommand === 'check'
  ? format(
      "%s check --strip-root --json %s %s",
      bin,
      includeWarnings,
      root,
    )
  : format(
      "%s status --no-auto-start --strip-root --json %s %s",
      bin,
      includeWarnings,
      root,
    )
  const [err, stdout, stderr] = await execManual(
    cmd,
    {cwd: root, maxBuffer: Infinity}
  );

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
): Promise<FlowResult> {
  return getFlowErrors(bin, errorCheckCommand, root, true);
}

export default async function(
  bin: string,
  errorCheckCommand: 'check' | 'status',
  root: string,
): Promise<FlowResult> {
  return getFlowErrors(bin, errorCheckCommand, root, false);
}
