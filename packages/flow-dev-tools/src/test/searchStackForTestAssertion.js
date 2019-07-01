/**
 * @flow
 * @format
 */

import {sync as resolve} from 'resolve';
import {dirname} from 'path';

import type {AssertionLocation} from './assertions/assertionTypes';

/* We need to use source-map-support in order to extract the right locations
 * from a stack trace. Unfortunately, we need to use the exact same version
 * that babel-register uses. We can use resolve() to find it.
 */
let wrapCallSite = null;
function getWrapCallSite() {
  if (wrapCallSite == null) {
    const babelRegisterPath = resolve('babel-register');
    const sourceMapSupportPath = resolve('source-map-support', {
      basedir: dirname(babelRegisterPath),
    });
    // $FlowFixMe - This is necessary :(
    wrapCallSite = require(sourceMapSupportPath).wrapCallSite;
  }
  return wrapCallSite;
}

export default function(): ?AssertionLocation {
  const oldPrepareStackTrace = Error.prepareStackTrace;

  const wrapCallSite = getWrapCallSite();
  Error.prepareStackTrace = (_, stack) => stack.map(wrapCallSite);
  const stack: Array<Object> = (new Error().stack: any);
  Error.prepareStackTrace = oldPrepareStackTrace;

  for (const callSite of stack) {
    const filename = callSite.getFileName();
    if (filename != null && filename.match(/test.js$/)) {
      return {
        filename,
        line: callSite.getLineNumber(),
        column: callSite.getColumnNumber(),
      };
    }
  }
  return null;
}
