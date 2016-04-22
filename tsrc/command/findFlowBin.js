/* @flow */

import {existsSync} from 'fs';
import {resolve} from 'path';

import {binOptions} from './../constants';

export default function(binArg: ?string): string {
  // Command line arg wins
  if (binArg != null) {
    return resolve(binArg);
  }

  for (const option of binOptions) {
    if (existsSync(option)) {
      return option;
    }
  }


  // Default to whatever is in the path
  return "flow";
}
