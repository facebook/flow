/*
 * @flow
 */


import type {Suite} from "flow-dev-tools/src/test/Suite";
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(({addFile, addFiles, addCode}) => [
  test('Munged instance fields does not require annotation within init values', [
    addCode('export class Foo { _a = (p) => 42; }')
      .noNewErrors(),
  ]),

  test('Munged instance methods does not require annotation', [
    addCode('export class Foo { _a(p) {} }')
      .noNewErrors(),
  ]),

]): Suite);
