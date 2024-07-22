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

  addFile("exact.js"),
  flowCmd(
    ['autocomplete', '--strip-root', '--json', 'exact.js', '7', '5'],
    'exact.js',
  ).stdout(
     `
       {
         "result": [
           {
             "name": "num",
             "type": "number"
           },
           {
             "name": "str",
             "type": "string"
           }
         ]
       }
     `,
   ).exitCodes([0]),

   addFile("exact.js"),
   flowCmd(
     ['autocomplete', '--strip-root', '--json', 'exact.js', '6', '1'],
     'exact.js',
   ).stdout(
      `
        {
          "result": [
            {
              "name": "async",
              "type": ""
            },
            {
              "name": "await",
              "type": ""
            },
            {
              "name": "break",
              "type": ""
            },
            {
              "name": "case",
              "type": ""
            },
            {
              "name": "catch",
              "type": ""
            },
            {
              "name": "class",
              "type": ""
            },
            {
              "name": "const",
              "type": ""
            },
            {
              "name": "continue",
              "type": ""
            },
            {
              "name": "debugger",
              "type": ""
            },
            {
              "name": "declare",
              "type": ""
            },
            {
              "name": "default",
              "type": ""
            },
            {
              "name": "delete",
              "type": ""
            },
            {
              "name": "do",
              "type": ""
            },
            {
              "name": "else",
              "type": ""
            },
            {
              "name": "enum",
              "type": ""
            },
            {
              "name": "export",
              "type": ""
            },
            {
              "name": "extends",
              "type": ""
            },
            {
              "name": "finally",
              "type": ""
            },
            {
              "name": "for",
              "type": ""
            },
            {
              "name": "function",
              "type": ""
            },
            {
              "name": "if",
              "type": ""
            },
            {
              "name": "implements",
              "type": ""
            },
            {
              "name": "import",
              "type": ""
            },
            {
              "name": "interface",
              "type": ""
            },
            {
              "name": "let",
              "type": ""
            },
            {
              "name": "new",
              "type": ""
            },
            {
              "name": "obj",
              "type": "{|num: number, str: string|}"
            },
            {
              "name": "opaque",
              "type": ""
            },
            {
              "name": "return",
              "type": ""
            },
            {
              "name": "static",
              "type": ""
            },
            {
              "name": "switch",
              "type": ""
            },
            {
              "name": "throw",
              "type": ""
            },
            {
              "name": "try",
              "type": ""
            },
            {
              "name": "type",
              "type": ""
            },
            {
              "name": "typeof",
              "type": ""
            },
            {
              "name": "var",
              "type": ""
            },
            {
              "name": "void",
              "type": ""
            },
            {
              "name": "while",
              "type": ""
            },
            {
              "name": "yield",
              "type": ""
            }
          ]
        }
      `,
    ).exitCodes([0]),
  ]),
]): SuiteType);
