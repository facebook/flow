/*
 * @flow
 * @lint-ignore-every LINEWRAP1
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
         hasOwnProperty (prop: any) => boolean
         isPrototypeOf (o: any) => boolean
         num number
         propertyIsEnumerable (prop: any) => boolean
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
               "type": "(prop: any) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 66,
               "endline": 66,
               "start": 5,
               "end": 38
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: any) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "o",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 67,
               "endline": 67,
               "start": 5,
               "end": 34
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
               "type": "(prop: any) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 68,
               "endline": 68,
               "start": 5,
               "end": 44
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
               "line": 69,
               "endline": 69,
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
               "line": 70,
               "endline": 70,
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
               "line": 71,
               "endline": 71,
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
               "line": 295,
               "endline": 295,
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
               "line": 296,
               "endline": 296,
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
               "line": 297,
               "endline": 297,
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
               "line": 298,
               "endline": 298,
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
               "line": 299,
               "endline": 299,
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
               "line": 300,
               "endline": 300,
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
               "line": 302,
               "endline": 302,
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
               "line": 303,
               "endline": 303,
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
               "line": 304,
               "endline": 304,
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
               "line": 305,
               "endline": 305,
               "start": 5,
               "end": 64
             },
             {
               "name": "length",
               "type": "number",
               "func_details": null,
               "path": "[LIB] core.js",
               "line": 331,
               "endline": 331,
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
               "line": 306,
               "endline": 306,
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
               "line": 307,
               "endline": 307,
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
               "line": 308,
               "endline": 308,
               "start": 5,
               "end": 61
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
               "line": 309,
               "endline": 309,
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
               "line": 310,
               "endline": 310,
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
               "line": 311,
               "endline": 311,
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
               "line": 312,
               "endline": 312,
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
               "line": 313,
               "endline": 313,
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
               "line": 314,
               "endline": 314,
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
               "line": 315,
               "endline": 315,
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
               "line": 316,
               "endline": 316,
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
               "line": 317,
               "endline": 317,
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
               "line": 318,
               "endline": 318,
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
               "line": 319,
               "endline": 319,
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
               "line": 320,
               "endline": 320,
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
               "line": 321,
               "endline": 321,
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
               "line": 322,
               "endline": 322,
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
               "line": 330,
               "endline": 330,
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
               "line": 323,
               "endline": 323,
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
               "line": 324,
               "endline": 324,
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
               "line": 325,
               "endline": 325,
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
               "line": 326,
               "endline": 326,
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
               "line": 327,
               "endline": 327,
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
               "line": 328,
               "endline": 328,
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
               "line": 329,
               "endline": 329,
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
               "line": 144,
               "endline": 144,
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
               "line": 145,
               "endline": 145,
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
               "line": 146,
               "endline": 146,
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
               "line": 147,
               "endline": 147,
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
               "line": 148,
               "endline": 148,
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
               "line": 149,
               "endline": 149,
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
               "line": 124,
               "endline": 124,
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
               "line": 123,
               "endline": 123,
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
               "type": "(prop: any) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 66,
               "endline": 66,
               "start": 5,
               "end": 38
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: any) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "o",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 67,
               "endline": 67,
               "start": 5,
               "end": 34
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: any) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 68,
               "endline": 68,
               "start": 5,
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
               "line": 69,
               "endline": 69,
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
               "line": 70,
               "endline": 70,
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
               "line": 71,
               "endline": 71,
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
           "result": []
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
               "line": 144,
               "endline": 144,
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
               "line": 145,
               "endline": 145,
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
               "line": 146,
               "endline": 146,
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
               "line": 147,
               "endline": 147,
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
               "line": 148,
               "endline": 148,
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
               "line": 149,
               "endline": 149,
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
               "type": "(prop: any) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 66,
               "endline": 66,
               "start": 5,
               "end": 38
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: any) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "o",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 67,
               "endline": 67,
               "start": 5,
               "end": 34
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: any) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 68,
               "endline": 68,
               "start": 5,
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
               "line": 69,
               "endline": 69,
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
               "line": 70,
               "endline": 70,
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
               "line": 71,
               "endline": 71,
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
               "type": "(prop: any) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 66,
               "endline": 66,
               "start": 5,
               "end": 38
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: any) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "o",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 67,
               "endline": 67,
               "start": 5,
               "end": 34
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
               "type": "(prop: any) => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": [
                   {
                     "name": "prop",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 68,
               "endline": 68,
               "start": 5,
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
               "line": 69,
               "endline": 69,
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
               "line": 70,
               "endline": 70,
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
               "line": 71,
               "endline": 71,
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
      ['autocomplete', '--strip-root', '--json', 'jsx1.js', '8', '4'],
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
               "line": 6,
               "endline": 6,
               "start": 15,
               "end": 20
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("jsx2.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', 'jsx2.js', '8', '11'],
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
               "line": 6,
               "endline": 6,
               "start": 15,
               "end": 20
             },
             {
               "name": "y",
               "type": "string",
               "func_details": null,
               "path": "jsx2.js",
               "line": 6,
               "endline": 6,
               "start": 26,
               "end": 31
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
             },
             {
               "name": "exports",
               "type": "{||}",
               "func_details": null,
               "path": "customfun.js",
               "line": 0,
               "endline": 0,
               "start": 1,
               "end": 0
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
             "type": "(prop: any) => boolean",
             "func_details": {
               "return_type": "boolean",
               "params": [
                 {
                   "name": "prop",
                   "type": "any"
                 }
               ]
             },
             "path": "[LIB] core.js",
             "line": 66,
             "endline": 66,
             "start": 5,
             "end": 38
           },
           {
             "name": "isPrototypeOf",
             "type": "(o: any) => boolean",
             "func_details": {
               "return_type": "boolean",
               "params": [
                 {
                   "name": "o",
                   "type": "any"
                 }
               ]
             },
             "path": "[LIB] core.js",
             "line": 67,
             "endline": 67,
             "start": 5,
             "end": 34
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
             "type": "(prop: any) => boolean",
             "func_details": {
               "return_type": "boolean",
               "params": [
                 {
                   "name": "prop",
                   "type": "any"
                 }
               ]
             },
             "path": "[LIB] core.js",
             "line": 68,
             "endline": 68,
             "start": 5,
             "end": 44
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
             "line": 69,
             "endline": 69,
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
             "line": 70,
             "endline": 70,
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
             "line": 71,
             "endline": 71,
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
            },
            {
              "name": "exports",
              "type": "{||}",
              "func_details": null,
              "path": "exact.js",
              "line": 0,
              "endline": 0,
              "start": 1,
              "end": 0
            }
          ]
        }
      `,
    ).exitCodes([0]),
  ]),
]);
