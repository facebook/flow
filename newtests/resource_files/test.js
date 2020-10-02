/*
 * @flow
 */


import type Suite from "flow-dev-tools/src/test/Suite.js";
import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default (suite(({addFile, addFiles, addCode}) => [
  test('Requiring a .css file', [
    addFile('foo.css')
      .addCode("import './foo.css'")
      .noNewErrors(),
  ]),

  test('.css extension cannot be omitted', [
    addFile('foo.css')
      .addCode("import './foo'")
      .newErrors(
        `
          test.js:3
            3: import './foo'
                      ^^^^^^^ Cannot resolve module \`./foo\`. [cannot-resolve-module]
        `,
      ),
  ]),

  test('By default, .css files export the Object type', [
    addFile('foo.css')
      .addCode("const css = require('./foo.css');")
      .addCode("(css: string)")
      .noNewErrors(),
  ]),

  test('Typical use of a .css file', [
    addFile('foo.css')
      .addCode("import { active, hovered } from './foo.css';")
      .addCode("(active: string);")
      .addCode("(hovered: number);")
        .noNewErrors()
        .because("active and hovered have the type any"),
  ]),

  test('Requiring a .png file', [
    addFile('bar.png')
      .addCode("import './bar.png'")
      .noNewErrors(),
  ]),

  test('.png extension cannot be omitted', [
    addFile('bar.png')
      .addCode("import './bar'")
      .newErrors(
        `
          test.js:3
            3: import './bar'
                      ^^^^^^^ Cannot resolve module \`./bar\`. [cannot-resolve-module]
        `,
      ),
  ]),

  test('By default, .png files export the string type', [
    addFile('bar.png')
      .addCode("const png = require('./bar.png');")
      .addCode("(png: number)")
      .newErrors(
        `
          test.js:5
            5: (png: number)
                ^^^ Cannot cast \`png\` to number because string [1] is incompatible with number [2]. [incompatible-cast]
            References:
              3: const png = require('./bar.png');
                                     ^^^^^^^^^^^ [1]
              5: (png: number)
                       ^^^^^^ [2]
        `,
      ),
  ]),

  test('module.name_mapper should still work', [
    addFiles('foo.css', 'cssMock.js')
      .addCode('const css = require("./foo.css");')
      .addCode('(css: string)')
      .newErrors(
        `
          test.js:5
            5: (css: string)
                ^^^ Cannot cast \`css\` to string because boolean [1] is incompatible with string [2]. [incompatible-cast]
            References:
              2: declare module.exports: boolean;
                                         ^^^^^^^ [1]. See: cssMock.js:2
              5: (css: string)
                       ^^^^^^ [2]
        `,
      ),
  ]).flowConfig('_flowconfig_with_module_name_mapper'),
]): Suite);
