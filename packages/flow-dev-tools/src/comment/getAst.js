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
import {exec} from '../utils/async';

export default async function(
  code: string,
  flowBinPath: string,
): Promise<Object> /* AST */ {
  const stdout = await exec(format('%s ast', flowBinPath), {
    maxBuffer: 16 * 1024 * 1024,
    stdin: code,
  });
  return JSON.parse(stdout);
}
