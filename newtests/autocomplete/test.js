/*
 * @flow
 */


import type {SuiteType} from "../Tester";
const {suite, test} = require('../Tester');

module.exports = (suite(({addFile, flowCmd}) => [
  test('non-json output', [
    addFile("qux.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'qux.js', '6', '3'],
      'qux.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "x",
               "type": "number"
             }
           ]
         }
       `,
     ).exitCodes([0]),
  ]),
]): SuiteType);
