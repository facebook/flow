/* @flow */

import {wrapCallSite} from 'babel-register/node_modules/source-map-support';

import type {AssertionLocation} from './assertions/assertionTypes';

export default function(): ?AssertionLocation {
  const oldPrepareStackTrace = Error.prepareStackTrace;
  Error.prepareStackTrace = (_, stack) => stack.map(wrapCallSite);
  const stack: Array<Object> = ((new Error()).stack: any);
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
