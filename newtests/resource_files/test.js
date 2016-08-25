/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
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
                      ^^^^^^^ ./foo. Required module not found
        `,
      ),
  ]),

  test('By default, .css files export the Object type', [
    addFile('foo.css')
      .addCode("const css = require('./foo.css');")
      .addCode("(css: string)")
      .newErrors(
        `
          test.js:5
            5: (css: string)
                ^^^ Flow assumes requiring a .css file returns an Object. This type is incompatible with
            5: (css: string)
                     ^^^^^^ string
        `,
      ),
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
                      ^^^^^^^ ./bar. Required module not found
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
                ^^^ string. This type is incompatible with
            5: (png: number)
                     ^^^^^^ number
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
                ^^^ boolean. This type is incompatible with
            5: (css: string)
                     ^^^^^^ string
        `,
      ),
  ]).flowConfig('_flowconfig_with_module_name_mapper'),
]);
