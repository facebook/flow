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


    addFile("jsx1.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'jsx1.js', '7', '4'],
      'jsx1.js',
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


    addFile("jsx2.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'jsx2.js', '7', '9'],
      'jsx2.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "y",
               "type": "string"
             }
           ]
         }
       `,
     ).exitCodes([0]),
  ]),
]): SuiteType);
