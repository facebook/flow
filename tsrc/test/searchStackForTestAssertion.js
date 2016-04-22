/* @flow */

import {wrapCallSite} from 'babel-register/node_modules/source-map-support';

import type {AssertionLocation} from './assertions/assertionTypes';

export default function(): ?AssertionLocation {
  // $FlowFixMe - add prepareStackTrace to lib file
  const oldPrepareStackTrace = Error.prepareStackTrace;
  // $FlowFixMe - add prepareStackTrace to lib file
  Error.prepareStackTrace = (_, stack) => stack.map(wrapCallSite);
  const stack: Array<Object> = ((new Error()).stack: any);
  // $FlowFixMe - add prepareStackTrace to lib file
  Error.prepareStackTrace = oldPrepareStackTrace;

  for (const callSite of stack) {
    const filename = callSite.getFileName();
    if (filename.match(/test.js$/)) {
      return {
        filename,
        line: callSite.getLineNumber(),
        column: callSite.getColumnNumber(),
      };
    }
  }
  return null;
}
