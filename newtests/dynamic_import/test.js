/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('returns a promise of the ModuleNamespaceObject', [
    addFile('esmodule.js').noNewErrors(),

    addCode(`
      async function f() {
        const esmodule = await import('./esmodule');
        (esmodule.pi: number);
      }
    `).noNewErrors(),

    addCode(`
      async function g() {
        const esmodule = await import('./esmodule');
        (esmodule.pi: string);
        esmodule.default;
      }
    `).newErrors(
        `
          test.js:13
           13:         (esmodule.pi: string);
                        ^^^^^^^^^^^ number. This type is incompatible with
           13:         (esmodule.pi: string);
                                     ^^^^^^ string

          test.js:14
           14:         esmodule.default;
                                ^^^^^^^ property \`default\`. Property not found in
           14:         esmodule.default;
                       ^^^^^^^^ exports of \`./esmodule\`
        `,
      ),

    addCode(`
      async function h() {
        const esmodule = import('./esmodule');
        (esmodule.pi: string);
      }
    `).newErrors(`
      test.js:21
       21:         (esmodule.pi: string);
                             ^^ property ${'`'}pi${'`'}. Property not found in
       21:         (esmodule.pi: string);
                    ^^^^^^^^ Promise
    `),
  ]),

  test('properly converts CJS modules to ModuleNamespaceObjects', [
    addFile('cjsmodule.js').noNewErrors(),

    addCode(`
      async function f() {
        const cjsmodule = await import('./cjsmodule');
        (cjsmodule.pi: number);
        (cjsmodule.default: {pi: number});
      }
    `).noNewErrors(),

    addCode(`
      async function g() {
        const cjsmodule = await import('./cjsmodule');
        (cjsmodule.pi: string);
        (cjsmodule.default: number);
      }
    `).newErrors(`
      test.js:14
       14:         (cjsmodule.pi: string);
                    ^^^^^^^^^^^^ number. This type is incompatible with
       14:         (cjsmodule.pi: string);
                                  ^^^^^^ string
      test.js:15
       15:         (cjsmodule.default: number);
                    ^^^^^^^^^^^^^^^^^ object literal. This type is incompatible with
       15:         (cjsmodule.default: number);
                                       ^^^^^^ number
    `),
  ]),
]);
