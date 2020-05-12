/*
 * @flow
 */


import {suite, test} from 'flow-dev-tools/src/test/Tester';

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
                        ^^^^^^^^^^^ Cannot cast \`esmodule.pi\` to string because number [1] is incompatible with string [2]. [incompatible-cast]
            References:
              3: export const pi = 3.14;
                                   ^^^^ [1]. See: esmodule.js:3
             13:         (esmodule.pi: string);
                                       ^^^^^^ [2]

          test.js:14
           14:         esmodule.default;
                                ^^^^^^^ Cannot get \`esmodule.default\` because property \`default\` is missing in module \`./esmodule\` [1]. [prop-missing]
            References:
             12:         const esmodule = await import('./esmodule');
                                                ^^^^^^^^^^^^^^^^^^^^ [1]
        `,
      ),

    addCode(`
      async function h() {
        const esmodule = import('./esmodule');
        (esmodule.pi: string);
      }
    `).newErrors(
        `
          test.js:21
           21:         (esmodule.pi: string);
                                 ^^ Cannot get \`esmodule.pi\` because property \`pi\` is missing in \`Promise\` [1]. [prop-missing]
            References:
             20:         const esmodule = import('./esmodule');
                                          ^^^^^^^^^^^^^^^^^^^^ [1]
        `,
      ),
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
                        ^^^^^^^^^^^^ Cannot cast \`cjsmodule.pi\` to string because number [1] is incompatible with string [2]. [incompatible-cast]
            References:
              4:   pi: 3.14
                       ^^^^ [1]. See: cjsmodule.js:4
             14:         (cjsmodule.pi: string);
                                        ^^^^^^ [2]

          test.js:15
           15:         (cjsmodule.default: number);
                        ^^^^^^^^^^^^^^^^^ Cannot cast \`cjsmodule.default\` to number because object literal [1] is incompatible with number [2]. [incompatible-cast]
            References:
              3: module.exports = {
                                  ^ [1]. See: cjsmodule.js:3
             15:         (cjsmodule.default: number);
                                             ^^^^^^ [2]
        `,
      ),
  ]),
]);
