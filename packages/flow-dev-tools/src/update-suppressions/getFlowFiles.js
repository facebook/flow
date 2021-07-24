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
import {execManual} from '../utils/async';

export default async function(
  bin: string,
  root: string,
  flowconfigName: string,
): Promise<Array<string>> {
  const flowconfigNameFlag = '--flowconfig-name ' + flowconfigName;
  // DO NOT pass the root as an arg! that filters to only files within the root,
  // excluding any [include]'d files outside the root.
  const cmd = format('%s ls %s', bin, flowconfigNameFlag);
  const [err, stdout, stderr] = await execManual(cmd, {
    cwd: root,
    maxBuffer: Infinity,
  });

  // 0 - no errors
  if (err == null) {
    return stdout.toString().split('\n');
  }

  throw new Error(format('flow ls failed!', err, stdout, stderr));
}
