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
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean"
             },
             {
               "name": "num",
               "type": "number"
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "str",
               "type": "string"
             },
             {
               "name": "toLocaleString",
               "type": "() => string"
             },
             {
               "name": "toString",
               "type": "() => string"
             },
             {
               "name": "valueOf",
               "type": "() => mixed"
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
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean"
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "toLocaleString",
               "type": "() => string"
             },
             {
               "name": "toString",
               "type": "() => string"
             },
             {
               "name": "valueOf",
               "type": "() => mixed"
             },
             {
               "name": "x",
               "type": "number"
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
               "type": "() => Iterator<string>"
             },
             {
               "name": "anchor",
               "type": "(name: string) => string"
             },
             {
               "name": "charAt",
               "type": "(pos: number) => string"
             },
             {
               "name": "charCodeAt",
               "type": "(index: number) => number"
             },
             {
               "name": "codePointAt",
               "type": "(index: number) => number"
             },
             {
               "name": "concat",
               "type": "(...strings: Array<string>) => string"
             },
             {
               "name": "endsWith",
               "type": "(searchString: string, position?: number) => boolean"
             },
             {
               "name": "hasOwnProperty",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "includes",
               "type": "(searchString: string, position?: number) => boolean"
             },
             {
               "name": "indexOf",
               "type": "(searchString: string, position?: number) => number"
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean"
             },
             {
               "name": "lastIndexOf",
               "type": "(searchString: string, position?: number) => number"
             },
             {
               "name": "length",
               "type": "number"
             },
             {
               "name": "link",
               "type": "(href: string) => string"
             },
             {
               "name": "localeCompare",
               "type": "(that: string, locales?: (string | Array<string>), options?: Intl$CollatorOptions) => number"
             },
             {
               "name": "match",
               "type": "(regexp: (string | RegExp)) => (RegExp$matchResult | null)"
             },
             {
               "name": "matchAll",
               "type": "(regexp: (string | RegExp)) => Iterator<RegExp$matchResult>"
             },
             {
               "name": "normalize",
               "type": "(format?: string) => string"
             },
             {
               "name": "padEnd",
               "type": "(targetLength: number, padString?: string) => string"
             },
             {
               "name": "padStart",
               "type": "(targetLength: number, padString?: string) => string"
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "repeat",
               "type": "(count: number) => string"
             },
             {
               "name": "replace",
               "type": "(searchValue: (string | RegExp), replaceValue: (string | ((substring: string, ...args: Array<any>) => string))) => string"
             },
             {
               "name": "search",
               "type": "(regexp: (string | RegExp)) => number"
             },
             {
               "name": "slice",
               "type": "(start?: number, end?: number) => string"
             },
             {
               "name": "split",
               "type": "(separator?: (string | RegExp), limit?: number) => Array<string>"
             },
             {
               "name": "startsWith",
               "type": "(searchString: string, position?: number) => boolean"
             },
             {
               "name": "substr",
               "type": "(from: number, length?: number) => string"
             },
             {
               "name": "substring",
               "type": "(start: number, end?: number) => string"
             },
             {
               "name": "toLocaleLowerCase",
               "type": "(locale?: (string | Array<string>)) => string"
             },
             {
               "name": "toLocaleString",
               "type": "() => string"
             },
             {
               "name": "toLocaleUpperCase",
               "type": "(locale?: (string | Array<string>)) => string"
             },
             {
               "name": "toLowerCase",
               "type": "() => string"
             },
             {
               "name": "toString",
               "type": "() => string"
             },
             {
               "name": "toUpperCase",
               "type": "() => string"
             },
             {
               "name": "trim",
               "type": "() => string"
             },
             {
               "name": "trimEnd",
               "type": "() => string"
             },
             {
               "name": "trimLeft",
               "type": "() => string"
             },
             {
               "name": "trimRight",
               "type": "() => string"
             },
             {
               "name": "trimStart",
               "type": "() => string"
             },
             {
               "name": "valueOf",
               "type": "() => string"
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
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean"
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "toExponential",
               "type": "(fractionDigits?: number) => string"
             },
             {
               "name": "toFixed",
               "type": "(fractionDigits?: number) => string"
             },
             {
               "name": "toLocaleString",
               "type": "(locales?: (string | Array<string>), options?: Intl$NumberFormatOptions) => string"
             },
             {
               "name": "toPrecision",
               "type": "(precision?: number) => string"
             },
             {
               "name": "toString",
               "type": "(radix?: number) => string"
             },
             {
               "name": "valueOf",
               "type": "() => number"
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
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean"
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "toLocaleString",
               "type": "() => string"
             },
             {
               "name": "toString",
               "type": "() => string"
             },
             {
               "name": "valueOf",
               "type": "() => boolean"
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
               "type": "string | any"
             },
             {
               "name": "hasOwnProperty",
               "type": "((prop: mixed) => boolean) | any"
             },
             {
               "name": "isPrototypeOf",
               "type": "((o: mixed) => boolean) | any"
             },
             {
               "name": "propertyIsEnumerable",
               "type": "((prop: mixed) => boolean) | any"
             },
             {
               "name": "toLocaleString",
               "type": "(() => string) | any"
             },
             {
               "name": "toString",
               "type": "(() => string) | any"
             },
             {
               "name": "valueOf",
               "type": "(() => mixed) | any"
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
               "type": "(thisArg: any, argArray?: any) => any"
             },
             {
               "name": "arguments",
               "type": "any"
             },
             {
               "name": "bind",
               "type": "(thisArg: any, ...argArray: Array<any>) => any"
             },
             {
               "name": "call",
               "type": "(thisArg: any, ...argArray: Array<any>) => any"
             },
             {
               "name": "caller",
               "type": "any | null"
             },
             {
               "name": "hasOwnProperty",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean"
             },
             {
               "name": "length",
               "type": "number"
             },
             {
               "name": "name",
               "type": "string"
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "toLocaleString",
               "type": "() => string"
             },
             {
               "name": "toString",
               "type": "() => string"
             },
             {
               "name": "valueOf",
               "type": "() => mixed"
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
               "type": "() => void"
             },
             {
               "name": "baz",
               "type": "string"
             },
             {
               "name": "hasOwnProperty",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "hello",
               "type": "() => void"
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean"
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "toLocaleString",
               "type": "() => string"
             },
             {
               "name": "toString",
               "type": "() => string"
             },
             {
               "name": "valueOf",
               "type": "() => mixed"
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
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean"
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "toExponential",
               "type": "(fractionDigits?: number) => string"
             },
             {
               "name": "toFixed",
               "type": "(fractionDigits?: number) => string"
             },
             {
               "name": "toLocaleString",
               "type": "(locales?: (string | Array<string>), options?: Intl$NumberFormatOptions) => string"
             },
             {
               "name": "toPrecision",
               "type": "(precision?: number) => string"
             },
             {
               "name": "toString",
               "type": "(radix?: number) => string"
             },
             {
               "name": "valueOf",
               "type": "() => number"
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
               "type": "C<number>"
             },
             {
               "name": "hasOwnProperty",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean"
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "toLocaleString",
               "type": "() => string"
             },
             {
               "name": "toString",
               "type": "() => string"
             },
             {
               "name": "valueOf",
               "type": "() => mixed"
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
               "type": "(x?: string) => void"
             },
             {
               "name": "hasOwnProperty",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean"
             },
             {
               "name": "o",
               "type": "{x?: string}"
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "toLocaleString",
               "type": "() => string"
             },
             {
               "name": "toString",
               "type": "() => string"
             },
             {
               "name": "valueOf",
               "type": "() => mixed"
             },
             {
               "name": "x",
               "type": "string | void"
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
               "type": "(o: any) => any"
             },
             {
               "name": "objectAssign",
               "type": "(target: any, ...sources: Array<any>) => any"
             },
             {
               "name": "idx",
               "type": "<IdxObject: any, IdxResult>(obj: IdxObject, pathCallback: (demaybefiedObj: IdxObject) => IdxResult) => ?IdxResult"
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
               "type": "string"
             },
             {
               "name": "hasOwnProperty",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "isPrototypeOf",
               "type": "(o: mixed) => boolean"
             },
             {
               "name": "method",
               "type": "() => void"
             },
             {
               "name": "prop",
               "type": "number"
             },
             {
               "name": "propertyIsEnumerable",
               "type": "(prop: mixed) => boolean"
             },
             {
               "name": "toLocaleString",
               "type": "() => string"
             },
             {
               "name": "toString",
               "type": "() => string"
             },
             {
               "name": "valueOf",
               "type": "() => mixed"
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
             "type": "(prop: mixed) => boolean"
           },
           {
             "name": "isPrototypeOf",
             "type": "(o: mixed) => boolean"
           },
           {
             "name": "num",
             "type": "number"
           },
           {
             "name": "propertyIsEnumerable",
             "type": "(prop: mixed) => boolean"
           },
           {
             "name": "str",
             "type": "string"
           },
           {
             "name": "toLocaleString",
             "type": "() => string"
           },
           {
             "name": "toString",
             "type": "() => string"
           },
           {
             "name": "valueOf",
             "type": "() => mixed"
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
              "type": "{|num: number, str: string|}"
            }
          ]
        }
      `,
    ).exitCodes([0]),
  ]),
]);
