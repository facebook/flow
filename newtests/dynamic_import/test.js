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
                        ^^^^^^^^^^^ Cannot cast property \`pi\` to string because number [1] is incompatible with string [2].
            References:
             13:         (esmodule.pi: string);
                          ^^^^^^^^^^^ [1]: number
             13:         (esmodule.pi: string);
                                       ^^^^^^ [2]: string

          test.js:14
           14:         esmodule.default;
                                ^^^^^^^ property \`default\`. Property not found in
           14:         esmodule.default;
                       ^^^^^^^^ module \`./esmodule\`
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
    `).newErrors(
        `
          test.js:14
           14:         (cjsmodule.pi: string);
                        ^^^^^^^^^^^^ Cannot cast property \`pi\` to string because number [1] is incompatible with string [2].
            References:
             14:         (cjsmodule.pi: string);
                          ^^^^^^^^^^^^ [1]: number
             14:         (cjsmodule.pi: string);
                                        ^^^^^^ [2]: string

          test.js:15
           15:         (cjsmodule.default: number);
                        ^^^^^^^^^^^^^^^^^ Cannot cast property \`default\` to number because object literal [1] is incompatible with number [2].
            References:
             15:         (cjsmodule.default: number);
                          ^^^^^^^^^^^^^^^^^ [1]: object literal
             15:         (cjsmodule.default: number);
                                             ^^^^^^ [2]: number
        `,
      ),
  ]),
]);
