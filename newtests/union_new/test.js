/*
 * @flow
 */


import type {Suite} from "flow-dev-tools/src/test/Suite";
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(({addFile, addFiles, addCode}) => [
  test('Functions with unions: https://github.com/facebook/flow/issues/1948', [
    addFile('issue-1948.js').noNewErrors(),
  ]),
]): Suite);
