/* @flow */

import {format} from 'util';
import {execManual} from '../async';

import type {FlowResult} from '../flowResult';

export default async function(
  bin: string,
  errorCheckCommand: 'check' | 'status',
  root: string,
): Promise<FlowResult> {
  const cmd = errorCheckCommand === 'check'
  ? format(
      "%s check --strip-root --json %s",
      bin,
      root,
    )
  : format(
      "%s status --no-auto-start --strip-root --json %s",
      bin,
      root,
    )
  const [err, stdout, stderr] = await execManual(
    cmd,
    {cwd: root, maxBuffer: 16 * 1024 * 1024}
  );

  // 0 - no errors
  // 2 - Some errors
  if (err == null || err.code === 2) {
    return JSON.parse(stdout.toString());
  }

  throw new Error(format('Flow check failed!', err, stdout, stderr));
}
