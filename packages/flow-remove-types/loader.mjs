// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

import {createRequire} from 'node:module';
import {fileURLToPath} from 'node:url';
const require = createRequire(import.meta.url);
const runnable = require('./runnable');
let options = {};
let sharedOptions = null;
export function initialize(data) {
  sharedOptions = data.shared;
}

export function globalPreload() {
  options = JSON.parse(process.env.FLOW_NODE_OPTIONS || '{}');
  return     'const require = getBuiltin("module").createRequire(' + JSON.stringify(import.meta.url) + ');' +
    'require("./register")(' + JSON.stringify(options) + ');';
}

export async function load(url, context, nextLoad) {
  if (sharedOptions != null) {
    options = runnable.readOptions(sharedOptions);
  }
  if (url === new URL('./flow-node', import.meta.url).href) {
    return nextLoad(url, {...context, format: 'commonjs'});
  }
  const result = await nextLoad(url, context);
  if (result.format !== 'module' || !url.startsWith('file:')) return result;
  const filename = fileURLToPath(url);
  if (!runnable.shouldTransform(filename, options)) return result;
  const emitted = runnable.compile(Buffer.from(result.source).toString('utf8'), filename, options, 'module');
  if (!emitted.map) return result;
  return {...result, source: runnable.inlineSourceMap(emitted)};
}
