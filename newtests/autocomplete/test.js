/*
 * @flow
 */


import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(({addFile, flowCmd}) => [
  test('non-json output', [
    addFile('foo_parse_fail.js'),
    flowCmd(
      ['autocomplete', '--strip-root', 'foo_parse_fail.js', '10', '17'],
      'foo_parse_fail.js',
    ).stdout(
       `
         hasOwnProperty (prop: mixed) => boolean
         isPrototypeOf (o: mixed) => boolean
         num number
         propertyIsEnumerable (prop: mixed) => boolean
         str string
         toLocaleString () => string
         toString () => string
         valueOf () => mixed

       `,
     ).exitCodes([0]),
  ]),

  test("json output", [
    addFile("foo.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'foo.js', '10', '5'],
      'foo.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "hasOwnProperty",
               "type": "(prop: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 80,
               "endline": 80,
               "start": 5,
               "end": 40
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "o",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 81,
               "endline": 81,
               "start": 5,
               "end": 36
             },
             {
               "name": "num",
               "type": "number",
               "func_details": null,
               "path": "foo.js",
               "line": 6,
               "endline": 6,
               "start": 8,
               "end": 10
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 82,
               "endline": 82,
               "start": 5,
               "end": 46
             },
             {
               "name": "str",
               "type": "string",
               "func_details": null,
               "path": "foo.js",
               "line": 7,
               "endline": 7,
               "start": 8,
               "end": 14
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 83,
               "endline": 83,
               "start": 5,
               "end": 28
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 84,
               "endline": 84,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => mixed",
               "func_details": {
                 "return_type": "mixed",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 85,
               "endline": 85,
               "start": 5,
               "end": 20
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("bar.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'bar.js', '4', '5'],
      'bar.js',
    ).stdout(
`{
  "error": "not enough type information to autocomplete",
  "result": []
}`,
).exitCodes([0]),


    addFile("qux.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'qux.js', '6', '3'],
      'qux.js',
    ).stdout(
`{
  "result": [
    {
      "name": "x",
      "type": "number",
      "func_details": null,
      "path": "qux.js",
      "line": 3,
      "endline": 3,
      "start": 14,
      "end": 19
    }
  ]
}`,
).exitCodes([0]),


    addFile("str.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'str.js', '3', '9'],
      'str.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "@@iterator",
               "type": "() => Iterator<string>",
               "func_details": {
                 "return_type": "Iterator<string>",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 321,
               "endline": 321,
               "start": 5,
               "end": 34
             },
             {
               "name": "anchor",
               "type": "(name: string) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "name",
                     "type": "string"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 322,
               "endline": 322,
               "start": 5,
               "end": 32
             },
             {
               "name": "charAt",
               "type": "(pos: number) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "pos",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 323,
               "endline": 323,
               "start": 5,
               "end": 31
             },
             {
               "name": "charCodeAt",
               "type": "(index: number) => number",
               "func_details": {
                 "return_type": "number",
                 "params": [
                   {
                     "name": "index",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 324,
               "endline": 324,
               "start": 5,
               "end": 37
             },
             {
               "name": "codePointAt",
               "type": "(index: number) => number",
               "func_details": {
                 "return_type": "number",
                 "params": [
                   {
                     "name": "index",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 325,
               "endline": 325,
               "start": 5,
               "end": 38
             },
             {
               "name": "concat",
               "type": "(...strings: Array<string>) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "...strings",
                     "type": "Array<string>"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 326,
               "endline": 326,
               "start": 5,
               "end": 45
             },
             {
               "name": "endsWith",
               "type": "(searchString: string, position?: number) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "searchString",
                     "type": "string"
                   },
                   {
                     "name": "position?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 328,
               "endline": 328,
               "start": 5,
               "end": 62
             },
             {
               "name": "includes",
               "type": "(searchString: string, position?: number) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "searchString",
                     "type": "string"
                   },
                   {
                     "name": "position?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 329,
               "endline": 329,
               "start": 5,
               "end": 62
             },
             {
               "name": "indexOf",
               "type": "(searchString: string, position?: number) => number",
               "func_details": {
                 "return_type": "number",
                 "params": [
                   {
                     "name": "searchString",
                     "type": "string"
                   },
                   {
                     "name": "position?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 330,
               "endline": 330,
               "start": 5,
               "end": 60
             },
             {
               "name": "lastIndexOf",
               "type": "(searchString: string, position?: number) => number",
               "func_details": {
                 "return_type": "number",
                 "params": [
                   {
                     "name": "searchString",
                     "type": "string"
                   },
                   {
                     "name": "position?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 331,
               "endline": 331,
               "start": 5,
               "end": 64
             },
             {
               "name": "length",
               "type": "number",
               "func_details": null,
               "path": "[LIB] core.js",
               "line": 358,
               "endline": 358,
               "start": 13,
               "end": 18
             },
             {
               "name": "link",
               "type": "(href: string) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "href",
                     "type": "string"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 332,
               "endline": 332,
               "start": 5,
               "end": 30
             },
             {
               "name": "localeCompare",
               "type": "(that: string, locales?: (string | Array<string>), options?: Intl$CollatorOptions) => number",
               "func_details": {
                 "return_type": "number",
                 "params": [
                   {
                     "name": "that",
                     "type": "string"
                   },
                   {
                     "name": "locales?",
                     "type": "string | Array<string>"
                   },
                   {
                     "name": "options?",
                     "type": "Intl$CollatorOptions"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 333,
               "endline": 333,
               "start": 5,
               "end": 105
             },
             {
               "name": "match",
               "type": "(regexp: (string | RegExp)) => (RegExp$matchResult | null)",
               "func_details": {
                 "return_type": "RegExp$matchResult | null",
                 "params": [
                   {
                     "name": "regexp",
                     "type": "string | RegExp"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 334,
               "endline": 334,
               "start": 5,
               "end": 61
             },
             {
               "name": "matchAll",
               "type": "(regexp: (string | RegExp)) => Iterator<RegExp$matchResult>",
               "func_details": {
                 "return_type": "Iterator<RegExp$matchResult>",
                 "params": [
                   {
                     "name": "regexp",
                     "type": "string | RegExp"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 335,
               "endline": 335,
               "start": 5,
               "end": 67
             },
             {
               "name": "normalize",
               "type": "(format?: string) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "format?",
                     "type": "string"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 336,
               "endline": 336,
               "start": 5,
               "end": 38
             },
             {
               "name": "padEnd",
               "type": "(targetLength: number, padString?: string) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "targetLength",
                     "type": "number"
                   },
                   {
                     "name": "padString?",
                     "type": "string"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 337,
               "endline": 337,
               "start": 5,
               "end": 60
             },
             {
               "name": "padStart",
               "type": "(targetLength: number, padString?: string) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "targetLength",
                     "type": "number"
                   },
                   {
                     "name": "padString?",
                     "type": "string"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 338,
               "endline": 338,
               "start": 5,
               "end": 62
             },
             {
               "name": "repeat",
               "type": "(count: number) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "count",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 339,
               "endline": 339,
               "start": 5,
               "end": 33
             },
             {
               "name": "replace",
               "type": "(searchValue: (string | RegExp), replaceValue: (string | ((substring: string, ...args: Array<any>) => string))) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "searchValue",
                     "type": "string | RegExp"
                   },
                   {
                     "name": "replaceValue",
                     "type": "string | ((substring: string, ...args: Array<any>) => string)"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 340,
               "endline": 340,
               "start": 5,
               "end": 124
             },
             {
               "name": "search",
               "type": "(regexp: (string | RegExp)) => number",
               "func_details": {
                 "return_type": "number",
                 "params": [
                   {
                     "name": "regexp",
                     "type": "string | RegExp"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 341,
               "endline": 341,
               "start": 5,
               "end": 43
             },
             {
               "name": "slice",
               "type": "(start?: number, end?: number) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "start?",
                     "type": "number"
                   },
                   {
                     "name": "end?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 342,
               "endline": 342,
               "start": 5,
               "end": 47
             },
             {
               "name": "split",
               "type": "(separator?: (string | RegExp), limit?: number) => Array<string>",
               "func_details": {
                 "return_type": "Array<string>",
                 "params": [
                   {
                     "name": "separator?",
                     "type": "string | RegExp"
                   },
                   {
                     "name": "limit?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 343,
               "endline": 343,
               "start": 5,
               "end": 69
             },
             {
               "name": "startsWith",
               "type": "(searchString: string, position?: number) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "searchString",
                     "type": "string"
                   },
                   {
                     "name": "position?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 344,
               "endline": 344,
               "start": 5,
               "end": 64
             },
             {
               "name": "substr",
               "type": "(from: number, length?: number) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "from",
                     "type": "number"
                   },
                   {
                     "name": "length?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 345,
               "endline": 345,
               "start": 5,
               "end": 49
             },
             {
               "name": "substring",
               "type": "(start: number, end?: number) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "start",
                     "type": "number"
                   },
                   {
                     "name": "end?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 346,
               "endline": 346,
               "start": 5,
               "end": 50
             },
             {
               "name": "toLocaleLowerCase",
               "type": "(locale?: (string | Array<string>)) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "locale?",
                     "type": "string | Array<string>"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 347,
               "endline": 347,
               "start": 5,
               "end": 62
             },
             {
               "name": "toLocaleUpperCase",
               "type": "(locale?: (string | Array<string>)) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "locale?",
                     "type": "string | Array<string>"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 348,
               "endline": 348,
               "start": 5,
               "end": 62
             },
             {
               "name": "toLowerCase",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 349,
               "endline": 349,
               "start": 5,
               "end": 25
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 357,
               "endline": 357,
               "start": 5,
               "end": 22
             },
             {
               "name": "toUpperCase",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 350,
               "endline": 350,
               "start": 5,
               "end": 25
             },
             {
               "name": "trim",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 351,
               "endline": 351,
               "start": 5,
               "end": 18
             },
             {
               "name": "trimEnd",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 352,
               "endline": 352,
               "start": 5,
               "end": 21
             },
             {
               "name": "trimLeft",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 353,
               "endline": 353,
               "start": 5,
               "end": 22
             },
             {
               "name": "trimRight",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 354,
               "endline": 354,
               "start": 5,
               "end": 23
             },
             {
               "name": "trimStart",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 355,
               "endline": 355,
               "start": 5,
               "end": 23
             },
             {
               "name": "valueOf",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 356,
               "endline": 356,
               "start": 5,
               "end": 21
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("num.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'num.js', '4', '5'],
      'num.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "toExponential",
               "type": "(fractionDigits?: number) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "fractionDigits?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 160,
               "endline": 160,
               "start": 5,
               "end": 50
             },
             {
               "name": "toFixed",
               "type": "(fractionDigits?: number) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "fractionDigits?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 161,
               "endline": 161,
               "start": 5,
               "end": 44
             },
             {
               "name": "toLocaleString",
               "type": "(locales?: (string | Array<string>), options?: Intl$NumberFormatOptions) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "locales?",
                     "type": "string | Array<string>"
                   },
                   {
                     "name": "options?",
                     "type": "Intl$NumberFormatOptions"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 162,
               "endline": 162,
               "start": 5,
               "end": 96
             },
             {
               "name": "toPrecision",
               "type": "(precision?: number) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "precision?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 163,
               "endline": 163,
               "start": 5,
               "end": 43
             },
             {
               "name": "toString",
               "type": "(radix?: number) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "radix?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 164,
               "endline": 164,
               "start": 5,
               "end": 36
             },
             {
               "name": "valueOf",
               "type": "() => number",
               "func_details": {
                 "return_type": "number",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 165,
               "endline": 165,
               "start": 5,
               "end": 21
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("bool.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'bool.js', '3', '6'],
      'bool.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 140,
               "endline": 140,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 139,
               "endline": 139,
               "start": 5,
               "end": 22
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("union.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'union.js', '10', '5'],
      'union.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "bar",
               "type": "string",
               "func_details": null,
               "path": "union.js",
               "line": 3,
               "endline": 3,
               "start": 36,
               "end": 41
             },
             {
               "name": "hasOwnProperty",
               "type": "(prop: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 80,
               "endline": 80,
               "start": 5,
               "end": 40
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "o",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 81,
               "endline": 81,
               "start": 5,
               "end": 36
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 82,
               "endline": 82,
               "start": 5,
               "end": 46
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 83,
               "endline": 83,
               "start": 5,
               "end": 28
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 84,
               "endline": 84,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => mixed",
               "func_details": {
                 "return_type": "mixed",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 85,
               "endline": 85,
               "start": 5,
               "end": 20
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("object_builtins.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'object_builtins.js', '4', '5'],
      'object_builtins.js',
    ).stdout(
       `
         {
           "error": "not enough type information to autocomplete",
           "result": []
         }
       `,
     ).exitCodes([0]),


    addFile("function_builtins.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'function_builtins.js', '4', '5'],
      'function_builtins.js',
    ).stdout(
       `
         {
           "error": "not enough type information to autocomplete",
           "result": []
         }
       `,
     ).exitCodes([0]),


    addFile("fun.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'fun.js', '4', '5'],
      'fun.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "hasOwnProperty",
               "type": "(prop: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 80,
               "endline": 80,
               "start": 5,
               "end": 40
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "o",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 81,
               "endline": 81,
               "start": 5,
               "end": 36
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 82,
               "endline": 82,
               "start": 5,
               "end": 46
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 83,
               "endline": 83,
               "start": 5,
               "end": 28
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 84,
               "endline": 84,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => mixed",
               "func_details": {
                 "return_type": "mixed",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 85,
               "endline": 85,
               "start": 5,
               "end": 20
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("this.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'this.js', '8', '10'],
      'this.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "bar",
               "type": "() => void",
               "func_details": {
                 "return_type": "void",
                 "params": []
               },
               "path": "this.js",
               "line": 6,
               "endline": 6,
               "start": 3,
               "end": 16
             },
             {
               "name": "baz",
               "type": "string",
               "func_details": null,
               "path": "this.js",
               "line": 5,
               "endline": 5,
               "start": 8,
               "end": 13
             },
             {
               "name": "hello",
               "type": "() => void",
               "func_details": {
                 "return_type": "void",
                 "params": []
               },
               "path": "this.js",
               "line": 7,
               "endline": 9,
               "start": 3,
               "end": 3
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("typeparams.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'typeparams.js', '6', '16'],
      'typeparams.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "toExponential",
               "type": "(fractionDigits?: number) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "fractionDigits?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 160,
               "endline": 160,
               "start": 5,
               "end": 50
             },
             {
               "name": "toFixed",
               "type": "(fractionDigits?: number) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "fractionDigits?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 161,
               "endline": 161,
               "start": 5,
               "end": 44
             },
             {
               "name": "toLocaleString",
               "type": "(locales?: (string | Array<string>), options?: Intl$NumberFormatOptions) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "locales?",
                     "type": "string | Array<string>"
                   },
                   {
                     "name": "options?",
                     "type": "Intl$NumberFormatOptions"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 162,
               "endline": 162,
               "start": 5,
               "end": 96
             },
             {
               "name": "toPrecision",
               "type": "(precision?: number) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "precision?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 163,
               "endline": 163,
               "start": 5,
               "end": 43
             },
             {
               "name": "toString",
               "type": "(radix?: number) => string",
               "func_details": {
                 "return_type": "string",
                 "params": [
                   {
                     "name": "radix?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 164,
               "endline": 164,
               "start": 5,
               "end": 36
             },
             {
               "name": "valueOf",
               "type": "() => number",
               "func_details": {
                 "return_type": "number",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 165,
               "endline": 165,
               "start": 5,
               "end": 21
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("generics.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'generics.js', '6', '5'],
      'generics.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "cn",
               "type": "C<number>",
               "func_details": null,
               "path": "generics.js",
               "line": 5,
               "endline": 5,
               "start": 23,
               "end": 31
             },
             {
               "name": "hasOwnProperty",
               "type": "(prop: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 80,
               "endline": 80,
               "start": 5,
               "end": 40
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "o",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 81,
               "endline": 81,
               "start": 5,
               "end": 36
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 82,
               "endline": 82,
               "start": 5,
               "end": 46
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 83,
               "endline": 83,
               "start": 5,
               "end": 28
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 84,
               "endline": 84,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => mixed",
               "func_details": {
                 "return_type": "mixed",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 85,
               "endline": 85,
               "start": 5,
               "end": 20
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("optional.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'optional.js', '4', '14'],
      'optional.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "f",
               "type": "(x?: string) => void",
               "func_details": {
                 "return_type": "void",
                 "params": [
                   {
                     "name": "x?",
                     "type": "string"
                   }
                 ]
               },
               "path": "optional.js",
               "line": 3,
               "endline": 3,
               "start": 36,
               "end": 55
             },
             {
               "name": "hasOwnProperty",
               "type": "(prop: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 80,
               "endline": 80,
               "start": 5,
               "end": 40
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "o",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 81,
               "endline": 81,
               "start": 5,
               "end": 36
             },
             {
               "name": "o",
               "type": "{x?: string}",
               "func_details": null,
               "path": "optional.js",
               "line": 3,
               "endline": 3,
               "start": 61,
               "end": 74
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "mixed"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 82,
               "endline": 82,
               "start": 5,
               "end": 46
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 83,
               "endline": 83,
               "start": 5,
               "end": 28
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 84,
               "endline": 84,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => mixed",
               "func_details": {
                 "return_type": "mixed",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 85,
               "endline": 85,
               "start": 5,
               "end": 20
             },
             {
               "name": "x",
               "type": "void | string",
               "func_details": null,
               "path": "optional.js",
               "line": 3,
               "endline": 3,
               "start": 25,
               "end": 30
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
               "type": "number",
               "func_details": null,
               "path": "jsx1.js",
               "line": 5,
               "endline": 5,
               "start": 37,
               "end": 42
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("jsx2.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'jsx2.js', '7', '11'],
      'jsx2.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "x",
               "type": "number",
               "func_details": null,
               "path": "jsx2.js",
               "line": 5,
               "endline": 5,
               "start": 37,
               "end": 42
             },
             {
               "name": "y",
               "type": "string",
               "func_details": null,
               "path": "jsx2.js",
               "line": 5,
               "endline": 5,
               "start": 48,
               "end": 53
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("customfun.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'customfun.js', '6', '1'],
      'customfun.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "objectGetPrototypeOf",
               "type": "(o: any) => any",
               "func_details": {
                 "return_type": "any",
                 "params": [
                   {
                     "name": "o",
                     "type": "any"
                   }
                 ]
               },
               "path": "customfun.js",
               "line": 4,
               "endline": 4,
               "start": 13,
               "end": 32
             },
             {
               "name": "objectAssign",
               "type": "(target: any, ...sources: Array<any>) => any",
               "func_details": {
                 "return_type": "any",
                 "params": [
                   {
                     "name": "target",
                     "type": "any"
                   },
                   {
                     "name": "...sources",
                     "type": "Array<any>"
                   }
                 ]
               },
               "path": "customfun.js",
               "line": 5,
               "endline": 5,
               "start": 13,
               "end": 24
             },
             {
               "name": "idx",
               "type": "<IdxObject: any, IdxResult>(obj: IdxObject, pathCallback: (demaybefiedObj: IdxObject) => IdxResult) => ?IdxResult",
               "func_details": {
                 "return_type": "?IdxResult",
                 "params": [
                   {
                     "name": "obj",
                     "type": "IdxObject"
                   },
                   {
                     "name": "pathCallback",
                     "type": "(demaybefiedObj: IdxObject) => IdxResult"
                   }
                 ]
               },
               "path": "customfun.js",
               "line": 3,
               "endline": 3,
               "start": 13,
               "end": 15
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("issue-1368.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'issue-1368.js', '20', '10'],
      'issue-1368.js',
    ).stdout(
`{
  "result": [
    {
      "name": "extended",
      "type": "string",
      "func_details": null,
      "path": "issue-1368.js",
      "line": 11,
      "endline": 11,
      "start": 13,
      "end": 18
    },
    {
      "name": "method",
      "type": "() => void",
      "func_details": {
        "return_type": "void",
        "params": []
      },
      "path": "issue-1368.js",
      "line": 18,
      "endline": 21,
      "start": 3,
      "end": 3
    },
    {
      "name": "prop",
      "type": "number",
      "func_details": null,
      "path": "issue-1368.js",
      "line": 3,
      "endline": 3,
      "start": 9,
      "end": 14
    }
  ]
}`,
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
             "name": "hasOwnProperty",
             "type": "(prop: mixed) => boolean",
             "func_details": {
               "return_type": "boolean",
               "params": [
                 {
                   "name": "prop",
                   "type": "mixed"
                 }
               ]
             },
             "path": "[LIB] core.js",
             "line": 80,
             "endline": 80,
             "start": 5,
             "end": 40
           },
           {
             "name": "isPrototypeOf",
             "type": "(o: mixed) => boolean",
             "func_details": {
               "return_type": "boolean",
               "params": [
                 {
                   "name": "o",
                   "type": "mixed"
                 }
               ]
             },
             "path": "[LIB] core.js",
             "line": 81,
             "endline": 81,
             "start": 5,
             "end": 36
           },
           {
             "name": "num",
             "type": "number",
             "func_details": null,
             "path": "exact.js",
             "line": 5,
             "endline": 5,
             "start": 26,
             "end": 31
           },
           {
             "name": "propertyIsEnumerable",
             "type": "(prop: mixed) => boolean",
             "func_details": {
               "return_type": "boolean",
               "params": [
                 {
                   "name": "prop",
                   "type": "mixed"
                 }
               ]
             },
             "path": "[LIB] core.js",
             "line": 82,
             "endline": 82,
             "start": 5,
             "end": 46
           },
           {
             "name": "str",
             "type": "string",
             "func_details": null,
             "path": "exact.js",
             "line": 5,
             "endline": 5,
             "start": 39,
             "end": 44
           },
           {
             "name": "toLocaleString",
             "type": "() => string",
             "func_details": {
               "return_type": "string",
               "params": []
             },
             "path": "[LIB] core.js",
             "line": 83,
             "endline": 83,
             "start": 5,
             "end": 28
           },
           {
             "name": "toString",
             "type": "() => string",
             "func_details": {
               "return_type": "string",
               "params": []
             },
             "path": "[LIB] core.js",
             "line": 84,
             "endline": 84,
             "start": 5,
             "end": 22
           },
           {
             "name": "valueOf",
             "type": "() => mixed",
             "func_details": {
               "return_type": "mixed",
               "params": []
             },
             "path": "[LIB] core.js",
             "line": 85,
             "endline": 85,
             "start": 5,
             "end": 20
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
              "name": "obj",
              "type": "{|num: number, str: string|}",
              "func_details": null,
              "path": "exact.js",
              "line": 5,
              "endline": 5,
              "start": 13,
              "end": 15
            }
          ]
        }
      `,
    ).exitCodes([0]),
  ]),
]);
