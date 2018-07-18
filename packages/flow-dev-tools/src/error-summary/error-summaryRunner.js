/* @flow
 * @format
 */

import getFlowErrors from '../comment/getFlowErrors';
import type {Args} from './error-summaryCommand';

export default (async function(args: Args): Promise<void> {
  var flow_result = await getFlowErrors(
    args.bin,
    args.errorCheckCommand,
    args.root,
  );

  var error_summary = {};
  flow_result.errors.forEach(error =>
    error.message.forEach(err => {
      if (err.descr in error_summary) {
        error_summary[err.descr] = error_summary[err.descr] + 1;
      } else {
        error_summary[err.descr] = 1;
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

  result.forEach(error =>
    console.log('frequency: ' + error.freq + ' message: ' + error.message),
  );
});
