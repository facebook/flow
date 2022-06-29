/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const flowJSPath = process.argv[2];
// js_of_ocaml uses the existence of process global to decide whether we are in node or browser
// environment.
// See https://github.com/ocsigen/js_of_ocaml/blob/3feaa1c6bf1647e670ebc8a8fabfda61a66aff8e/runtime/fs_node.js#L21-L26
// We want to smoke test behavior of flow.js in browser environment, so we trick it into believing
// that we are in browser.
global.process = undefined;
const flow = require(flowJSPath);

const libFile = 'lib.js';
flow.registerFile(libFile, `declare var MyGlobal: string;`);
flow.initBuiltins([libFile]);

if (flow.checkContent('test.js', 'MyGlobal;').length > 0) {
  throw 'There should be no errors if the library is correctly registered.';
}
if (
  flow.checkContent('test.js', 'MyGloba;')[0].message[0].descr !==
  'Cannot resolve name `MyGloba`. [cannot-resolve-name]'
) {
  throw 'Referring to non-existent global should be an error.';
}
