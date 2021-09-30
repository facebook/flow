/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {getFlowErrors} = require('../errors');
const {prettyPrintError} = require('../flowResult');
import type {Args} from './error-summaryCommand';

async function runner(args: Args): Promise<void> {
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
      let messageFilter =
        args.messageFilter == null ||
        new RegExp(args.messageFilter).test(err.descr);

      let codeFilter;
      if (args.codeFilter == null) {
        codeFilter = true;
      } else {
        const regexp = new RegExp(args.codeFilter);
        if (err.context != null) {
          codeFilter = regexp.test(err.context);
        } else {
          codeFilter = false;
        }
      }

      let fileFilter;
      if (args.fileFilter == null) {
        fileFilter = true;
      } else {
        const regexp = new RegExp(args.fileFilter);
        if (err.loc != null && err.loc.source != null) {
          codeFilter = regexp.test(err.loc.source);
        } else {
          codeFilter = false;
        }
      }

      if (messageFilter && codeFilter && fileFilter) {
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
}

module.exports = {
  default: runner,
};
