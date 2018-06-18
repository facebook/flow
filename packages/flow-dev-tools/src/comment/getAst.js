/* @flow */

import {format} from 'util';
import {exec} from '../utils/async';

export default async function(code: string, flowBinPath: string): Promise<Object> /* AST */ {
  const stdout = await exec(
    format("%s ast", flowBinPath),
    {
      maxBuffer: 16 * 1024 * 1024,
      stdin: code,
    },
  );
  return JSON.parse(stdout);
}
