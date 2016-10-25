/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

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
         valueOf () => Object

       `,
     ).exitCodes([0]),
  ]),

  test("json output", [
    addFile("foo.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'foo.js', '10', '5'],
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
               "line": 53,
               "endline": 53,
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
               "line": 54,
               "endline": 54,
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
               "line": 55,
               "endline": 55,
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
               "line": 56,
               "endline": 56,
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
               "line": 57,
               "endline": 57,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => Object",
               "func_details": {
                 "return_type": "Object",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 58,
               "endline": 58,
               "start": 5,
               "end": 21
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("bar.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'bar.js', '4', '5'],
      'bar.js',
    ).stdout(
`{
  "error": "not enough type information to autocomplete",
  "result": []
}`,
).exitCodes([0]),


    addFile("qux.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'qux.js', '6', '3'],
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
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'str.js', '3', '9'],
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
               "line": 240,
               "endline": 240,
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
               "line": 241,
               "endline": 241,
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
               "line": 242,
               "endline": 242,
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
               "line": 243,
               "endline": 243,
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
               "line": 244,
               "endline": 244,
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
               "line": 245,
               "endline": 245,
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
               "line": 246,
               "endline": 246,
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
               "line": 247,
               "endline": 247,
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
               "line": 248,
               "endline": 248,
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
               "line": 249,
               "endline": 249,
               "start": 5,
               "end": 64
             },
             {
               "name": "length",
               "type": "number",
               "func_details": null,
               "path": "[LIB] core.js",
               "line": 273,
               "endline": 273,
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
               "line": 250,
               "endline": 250,
               "start": 5,
               "end": 30
             },
             {
               "name": "localeCompare",
               "type": "(that: string) => number",
               "func_details": {
                 "return_type": "number",
                 "params": [
                   {
                     "name": "that",
                     "type": "string"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 251,
               "endline": 251,
               "start": 5,
               "end": 39
             },
             {
               "name": "match",
               "type": "(regexp: string | RegExp) => ?Array<string>",
               "func_details": {
                 "return_type": "?Array<string>",
                 "params": [
                   {
                     "name": "regexp",
                     "type": "string | RegExp"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 252,
               "endline": 252,
               "start": 5,
               "end": 50
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
               "line": 253,
               "endline": 253,
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
               "line": 254,
               "endline": 254,
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
               "line": 255,
               "endline": 255,
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
               "line": 256,
               "endline": 256,
               "start": 5,
               "end": 33
             },
             {
               "name": "replace",
               "type": "(searchValue: string | RegExp, replaceValue: string | ((substring: string, ...args: Array<any>) => string)) => string",
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
               "line": 257,
               "endline": 257,
               "start": 5,
               "end": 124
             },
             {
               "name": "search",
               "type": "(regexp: string | RegExp) => number",
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
               "line": 258,
               "endline": 258,
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
               "line": 259,
               "endline": 259,
               "start": 5,
               "end": 47
             },
             {
               "name": "split",
               "type": "(separator: string | RegExp, limit?: number) => Array<string>",
               "func_details": {
                 "return_type": "Array<string>",
                 "params": [
                   {
                     "name": "separator",
                     "type": "string | RegExp"
                   },
                   {
                     "name": "limit?",
                     "type": "number"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 260,
               "endline": 260,
               "start": 5,
               "end": 68
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
               "line": 261,
               "endline": 261,
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
               "line": 262,
               "endline": 262,
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
               "line": 263,
               "endline": 263,
               "start": 5,
               "end": 50
             },
             {
               "name": "toLocaleLowerCase",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 264,
               "endline": 264,
               "start": 5,
               "end": 31
             },
             {
               "name": "toLocaleUpperCase",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 265,
               "endline": 265,
               "start": 5,
               "end": 31
             },
             {
               "name": "toLowerCase",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 266,
               "endline": 266,
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
               "line": 272,
               "endline": 272,
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
               "line": 267,
               "endline": 267,
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
               "line": 268,
               "endline": 268,
               "start": 5,
               "end": 18
             },
             {
               "name": "trimLeft",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 269,
               "endline": 269,
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
               "line": 270,
               "endline": 270,
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
               "line": 271,
               "endline": 271,
               "start": 5,
               "end": 21
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("num.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'num.js', '4', '5'],
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
               "line": 128,
               "endline": 128,
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
               "line": 129,
               "endline": 129,
               "start": 5,
               "end": 44
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
               "line": 130,
               "endline": 130,
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
               "line": 131,
               "endline": 131,
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
               "line": 132,
               "endline": 132,
               "start": 5,
               "end": 21
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("bool.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'bool.js', '3', '6'],
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
               "line": 109,
               "endline": 109,
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
               "line": 108,
               "endline": 108,
               "start": 5,
               "end": 22
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("union.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'union.js', '10', '5'],
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
               "line": 53,
               "endline": 53,
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
               "line": 54,
               "endline": 54,
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
               "line": 55,
               "endline": 55,
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
               "line": 56,
               "endline": 56,
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
               "line": 57,
               "endline": 57,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => Object",
               "func_details": {
                 "return_type": "Object",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 58,
               "endline": 58,
               "start": 5,
               "end": 21
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("object_builtins.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'object_builtins.js', '4', '5'],
      'object_builtins.js',
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
               "line": 53,
               "endline": 53,
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
               "line": 54,
               "endline": 54,
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
               "line": 55,
               "endline": 55,
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
               "line": 56,
               "endline": 56,
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
               "line": 57,
               "endline": 57,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => Object",
               "func_details": {
                 "return_type": "Object",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 58,
               "endline": 58,
               "start": 5,
               "end": 21
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("function_builtins.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'function_builtins.js', '4', '5'],
      'function_builtins.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "apply",
               "type": "(thisArg: any, argArray?: any) => any",
               "func_details": {
                 "return_type": "any",
                 "params": [
                   {
                     "name": "thisArg",
                     "type": "any"
                   },
                   {
                     "name": "argArray?",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 97,
               "endline": 97,
               "start": 12,
               "end": 35
             },
             {
               "name": "arguments",
               "type": "any",
               "func_details": null,
               "path": "[LIB] core.js",
               "line": 100,
               "endline": 100,
               "start": 16,
               "end": 18
             },
             {
               "name": "bind",
               "type": "(thisArg: any, ...argArray: Array<any>) => any",
               "func_details": {
                 "return_type": "any",
                 "params": [
                   {
                     "name": "thisArg",
                     "type": "any"
                   },
                   {
                     "name": "...argArray",
                     "type": "Array<any>"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 98,
               "endline": 98,
               "start": 11,
               "end": 33
             },
             {
               "name": "call",
               "type": "(thisArg: any, ...argArray: Array<any>) => any",
               "func_details": {
                 "return_type": "any",
                 "params": [
                   {
                     "name": "thisArg",
                     "type": "any"
                   },
                   {
                     "name": "...argArray",
                     "type": "Array<any>"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 99,
               "endline": 99,
               "start": 11,
               "end": 33
             },
             {
               "name": "caller",
               "type": "null | Function",
               "func_details": null,
               "path": "[LIB] core.js",
               "line": 101,
               "endline": 101,
               "start": 13,
               "end": 27
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
               "line": 53,
               "endline": 53,
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
               "line": 54,
               "endline": 54,
               "start": 5,
               "end": 34
             },
             {
               "name": "length",
               "type": "number",
               "func_details": null,
               "path": "[LIB] core.js",
               "line": 102,
               "endline": 102,
               "start": 13,
               "end": 18
             },
             {
               "name": "name",
               "type": "string",
               "func_details": null,
               "path": "[LIB] core.js",
               "line": 103,
               "endline": 103,
               "start": 11,
               "end": 16
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
               "line": 55,
               "endline": 55,
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
               "line": 56,
               "endline": 56,
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
               "line": 57,
               "endline": 57,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => Object",
               "func_details": {
                 "return_type": "Object",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 58,
               "endline": 58,
               "start": 5,
               "end": 21
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("fun.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'fun.js', '4', '5'],
      'fun.js',
    ).stdout(
       `
         {
           "result": [
             {
               "name": "apply",
               "type": "(thisArg: any, argArray?: any) => any",
               "func_details": {
                 "return_type": "any",
                 "params": [
                   {
                     "name": "thisArg",
                     "type": "any"
                   },
                   {
                     "name": "argArray?",
                     "type": "any"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 97,
               "endline": 97,
               "start": 12,
               "end": 35
             },
             {
               "name": "arguments",
               "type": "any",
               "func_details": null,
               "path": "[LIB] core.js",
               "line": 100,
               "endline": 100,
               "start": 16,
               "end": 18
             },
             {
               "name": "bind",
               "type": "(thisArg: any, ...argArray: Array<any>) => any",
               "func_details": {
                 "return_type": "any",
                 "params": [
                   {
                     "name": "thisArg",
                     "type": "any"
                   },
                   {
                     "name": "...argArray",
                     "type": "Array<any>"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 98,
               "endline": 98,
               "start": 11,
               "end": 33
             },
             {
               "name": "call",
               "type": "(thisArg: any, ...argArray: Array<any>) => any",
               "func_details": {
                 "return_type": "any",
                 "params": [
                   {
                     "name": "thisArg",
                     "type": "any"
                   },
                   {
                     "name": "...argArray",
                     "type": "Array<any>"
                   }
                 ]
               },
               "path": "[LIB] core.js",
               "line": 99,
               "endline": 99,
               "start": 11,
               "end": 33
             },
             {
               "name": "caller",
               "type": "null | Function",
               "func_details": null,
               "path": "[LIB] core.js",
               "line": 101,
               "endline": 101,
               "start": 13,
               "end": 27
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
               "line": 53,
               "endline": 53,
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
               "line": 54,
               "endline": 54,
               "start": 5,
               "end": 34
             },
             {
               "name": "length",
               "type": "number",
               "func_details": null,
               "path": "[LIB] core.js",
               "line": 102,
               "endline": 102,
               "start": 13,
               "end": 18
             },
             {
               "name": "name",
               "type": "string",
               "func_details": null,
               "path": "[LIB] core.js",
               "line": 103,
               "endline": 103,
               "start": 11,
               "end": 16
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
               "line": 55,
               "endline": 55,
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
               "line": 56,
               "endline": 56,
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
               "line": 57,
               "endline": 57,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => Object",
               "func_details": {
                 "return_type": "Object",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 58,
               "endline": 58,
               "start": 5,
               "end": 21
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("this.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'this.js', '8', '10'],
      'this.js',
    ).stdout(
`{
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
      "end": 10
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
}`,
).exitCodes([0]),


    addFile("typeparams.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'typeparams.js', '6', '16'],
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
               "line": 128,
               "endline": 128,
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
               "line": 129,
               "endline": 129,
               "start": 5,
               "end": 44
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
               "line": 130,
               "endline": 130,
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
               "line": 131,
               "endline": 131,
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
               "line": 132,
               "endline": 132,
               "start": 5,
               "end": 21
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("generics.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'generics.js', '6', '5'],
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
               "line": 53,
               "endline": 53,
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
               "line": 54,
               "endline": 54,
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
               "line": 55,
               "endline": 55,
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
               "line": 56,
               "endline": 56,
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
               "line": 57,
               "endline": 57,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => Object",
               "func_details": {
                 "return_type": "Object",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 58,
               "endline": 58,
               "start": 5,
               "end": 21
             }
           ]
         }
       `,
     ).exitCodes([0]),


    addFile("optional.js"),
    flowCmd(
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'optional.js', '4', '14'],
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
               "line": 53,
               "endline": 53,
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
               "line": 54,
               "endline": 54,
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
               "line": 55,
               "endline": 55,
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
               "line": 56,
               "endline": 56,
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
               "line": 57,
               "endline": 57,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => Object",
               "func_details": {
                 "return_type": "Object",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 58,
               "endline": 58,
               "start": 5,
               "end": 21
             },
             {
               "name": "x",
               "type": "string | void",
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
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'jsx1.js', '8', '4'],
      'jsx1.js',
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
               "line": 53,
               "endline": 53,
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
               "line": 54,
               "endline": 54,
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
               "line": 55,
               "endline": 55,
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
               "line": 56,
               "endline": 56,
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
               "line": 57,
               "endline": 57,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => Object",
               "func_details": {
                 "return_type": "Object",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 58,
               "endline": 58,
               "start": 5,
               "end": 21
             },
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
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'jsx2.js', '8', '11'],
      'jsx2.js',
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
               "line": 53,
               "endline": 53,
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
               "line": 54,
               "endline": 54,
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
               "line": 55,
               "endline": 55,
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
               "line": 56,
               "endline": 56,
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
               "line": 57,
               "endline": 57,
               "start": 5,
               "end": 22
             },
             {
               "name": "valueOf",
               "type": "() => Object",
               "func_details": {
                 "return_type": "Object",
                 "params": []
               },
               "path": "[LIB] core.js",
               "line": 58,
               "endline": 58,
               "start": 5,
               "end": 21
             },
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
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'customfun.js', '11', '2'],
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
               "line": 8,
               "endline": 8,
               "start": 1,
               "end": 55
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
               "line": 9,
               "endline": 9,
               "start": 1,
               "end": 39
             },
             {
               "name": "mixin",
               "type": "(...objects: Array<Object>) => [class: Object]",
               "func_details": {
                 "return_type": "[class: Object]",
                 "params": [
                   {
                     "name": "...objects",
                     "type": "Array<Object>"
                   }
                 ]
               },
               "path": "customfun.js",
               "line": 7,
               "endline": 7,
               "start": 1,
               "end": 38
             },
             {
               "name": "mergeInto",
               "type": "(target: Object, ...objects: Array<Object>) => void",
               "func_details": {
                 "return_type": "void",
                 "params": [
                   {
                     "name": "target",
                     "type": "Object"
                   },
                   {
                     "name": "...objects",
                     "type": "Array<Object>"
                   }
                 ]
               },
               "path": "customfun.js",
               "line": 6,
               "endline": 6,
               "start": 1,
               "end": 46
             },
             {
               "name": "mergeDeepInto",
               "type": "(target: Object, ...objects: Array<Object>) => void",
               "func_details": {
                 "return_type": "void",
                 "params": [
                   {
                     "name": "target",
                     "type": "Object"
                   },
                   {
                     "name": "...objects",
                     "type": "Array<Object>"
                   }
                 ]
               },
               "path": "customfun.js",
               "line": 5,
               "endline": 5,
               "start": 1,
               "end": 54
             },
             {
               "name": "merge",
               "type": "(...objects: Array<Object>) => Object",
               "func_details": {
                 "return_type": "Object",
                 "params": [
                   {
                     "name": "...objects",
                     "type": "Array<Object>"
                   }
                 ]
               },
               "path": "customfun.js",
               "line": 4,
               "endline": 4,
               "start": 1,
               "end": 38
             },
             {
               "name": "idx",
               "type": "",
               "func_details": {
                 "return_type": "?IdxResult",
                 "params": [
                   {
                     "name": "obj",
                     "type": "IdxObject"
                   },
                   {
                     "name": "pathCallback",
                     "type": ""
                   }
                 ]
               },
               "path": "customfun.js",
               "line": 3,
               "endline": 3,
               "start": 1,
               "end": 34
             },
             {
               "name": "exports",
               "type": "{}",
               "func_details": null,
               "path": "",
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
      ['autocomplete', '--strip-root', '--json', '--no-auto-start', 'issue-1368.js', '20', '10'],
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
  ]),
]);
