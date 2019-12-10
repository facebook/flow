/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import getFlowErrors from '../comment/getFlowErrors';
import {prettyPrintError} from '../flowResult';
import type {Args} from './error-summaryCommand';

export default (async function(args: Args): Promise<void> {
  var flow_result = await getFlowErrors(
    args.bin,
    args.errorCheckCommand,
    args.root,
    args.flowconfigName,
  );

  var error_summary = {};
  var files = new Set();
  flow_result.errors.forEach(error =>
    error.message.forEach(err => {
      if (
        (args.messageFilter == null ||
          new RegExp(args.messageFilter).test(err.descr)) &&
        (args.codeFilter == null ||
          (err.context != null &&
            new RegExp(args.codeFilter).test(err.context))) &&
        (args.fileFilter == null ||
          (err.loc != null &&
            err.loc.source != null &&
            new RegExp(args.fileFilter).test(err.loc.source)))
      ) {
        if (args.showErrors) {
          console.log(prettyPrintError(error));
        }

        if (args.showFiles && err.loc != null && err.loc.source != null) {
          files.add(err.loc.source);
        }

        if (err.descr in error_summary) {
          error_summary[err.descr] = error_summary[err.descr] + 1;
        } else {
          error_summary[err.descr] = 1;
        }
      }
    }),
  );

  var result = Object.keys(error_summary).map(key => {
    return {
      message: key,
      freq: error_summary[key],
    };
  });
  result.sort((first, second) => second.freq - first.freq);

  if (args.showFiles) {
    files.forEach(file => console.log(file));
  }

  result.forEach(error =>
    console.log('frequency: ' + error.freq + ' message: ' + error.message),
  );
});
