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
               }
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
               }
             },
             {
               "name": "num",
               "type": "number",
               "func_details": null
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
               }
             },
             {
               "name": "str",
               "type": "string",
               "func_details": null
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "valueOf",
               "type": "() => mixed",
               "func_details": {
                 "return_type": "mixed",
                 "params": []
               }
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
               }
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
               }
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
               }
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "valueOf",
               "type": "() => mixed",
               "func_details": {
                 "return_type": "mixed",
                 "params": []
               }
             },
             {
               "name": "x",
               "type": "number",
               "func_details": null
             }
           ]
         }
       `,
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
             },
             {
               "name": "length",
               "type": "number",
               "func_details": null
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
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
               }
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
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
               }
             },
             {
               "name": "toLowerCase",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "toUpperCase",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "trim",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "trimEnd",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "trimLeft",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "trimRight",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "trimStart",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "valueOf",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
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
               }
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
               }
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
               }
             },
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
               }
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
               }
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
               }
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
               }
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
               }
             },
             {
               "name": "valueOf",
               "type": "() => number",
               "func_details": {
                 "return_type": "number",
                 "params": []
               }
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
               }
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
               }
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
               }
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "valueOf",
               "type": "() => boolean",
               "func_details": {
                 "return_type": "boolean",
                 "params": []
               }
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
               "type": "string | any",
               "func_details": null
             },
             {
               "name": "hasOwnProperty",
               "type": "((prop: mixed) => boolean) | any",
               "func_details": null
             },
             {
               "name": "isPrototypeOf",
               "type": "((o: mixed) => boolean) | any",
               "func_details": null
             },
             {
               "name": "propertyIsEnumerable",
               "type": "((prop: mixed) => boolean) | any",
               "func_details": null
             },
             {
               "name": "toLocaleString",
               "type": "(() => string) | any",
               "func_details": null
             },
             {
               "name": "toString",
               "type": "(() => string) | any",
               "func_details": null
             },
             {
               "name": "valueOf",
               "type": "(() => mixed) | any",
               "func_details": null
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
               }
             },
             {
               "name": "arguments",
               "type": "any",
               "func_details": null
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
               }
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
               }
             },
             {
               "name": "caller",
               "type": "any | null",
               "func_details": null
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
               }
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
               }
             },
             {
               "name": "length",
               "type": "number",
               "func_details": null
             },
             {
               "name": "name",
               "type": "string",
               "func_details": null
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
               }
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "valueOf",
               "type": "() => mixed",
               "func_details": {
                 "return_type": "mixed",
                 "params": []
               }
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
               }
             },
             {
               "name": "baz",
               "type": "string",
               "func_details": null
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
               }
             },
             {
               "name": "hello",
               "type": "() => void",
               "func_details": {
                 "return_type": "void",
                 "params": []
               }
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
               }
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
               }
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "valueOf",
               "type": "() => mixed",
               "func_details": {
                 "return_type": "mixed",
                 "params": []
               }
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
               }
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
               }
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
               }
             },
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
               }
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
               }
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
               }
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
               }
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
               }
             },
             {
               "name": "valueOf",
               "type": "() => number",
               "func_details": {
                 "return_type": "number",
                 "params": []
               }
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
               "func_details": null
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
               }
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
               }
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
               }
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "valueOf",
               "type": "() => mixed",
               "func_details": {
                 "return_type": "mixed",
                 "params": []
               }
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
               }
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
               }
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
               }
             },
             {
               "name": "o",
               "type": "{x?: string}",
               "func_details": null
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
               }
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "valueOf",
               "type": "() => mixed",
               "func_details": {
                 "return_type": "mixed",
                 "params": []
               }
             },
             {
               "name": "x",
               "type": "string | void",
               "func_details": null
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
               "func_details": null
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
               "type": "string",
               "func_details": null
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
               }
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
               }
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
               }
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
       `
         {
           "result": [
             {
               "name": "extended",
               "type": "string",
               "func_details": null
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
               }
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
               }
             },
             {
               "name": "method",
               "type": "() => void",
               "func_details": {
                 "return_type": "void",
                 "params": []
               }
             },
             {
               "name": "prop",
               "type": "number",
               "func_details": null
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
               }
             },
             {
               "name": "toLocaleString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "toString",
               "type": "() => string",
               "func_details": {
                 "return_type": "string",
                 "params": []
               }
             },
             {
               "name": "valueOf",
               "type": "() => mixed",
               "func_details": {
                 "return_type": "mixed",
                 "params": []
               }
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
             }
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
             }
           },
           {
             "name": "num",
             "type": "number",
             "func_details": null
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
             }
           },
           {
             "name": "str",
             "type": "string",
             "func_details": null
           },
           {
             "name": "toLocaleString",
             "type": "() => string",
             "func_details": {
               "return_type": "string",
               "params": []
             }
           },
           {
             "name": "toString",
             "type": "() => string",
             "func_details": {
               "return_type": "string",
               "params": []
             }
           },
           {
             "name": "valueOf",
             "type": "() => mixed",
             "func_details": {
               "return_type": "mixed",
               "params": []
             }
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
              "func_details": null
            }
          ]
        }
      `,
    ).exitCodes([0]),
  ]),
]);
