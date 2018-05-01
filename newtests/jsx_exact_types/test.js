/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */

import { suite, test } from 'flow-dev-tools/src/test/Tester';

export default suite(({ addFile, addFiles, addCode }) => {
  const base = addCode(`
    import * as React from 'react';
    const $unimplemented: any = 12;
    type ExactProps = {|a: string, b: number, c: boolean|};
    const ExactComponent: React.ComponentType<ExactProps> = $unimplemented;
  `);
  return [
    test('splatting object literal (correct)', [
      base
        .addCode(`
<ExactComponent {...{a: 'a', b: 1, c: false}} />;
        `)
        .noNewErrors(),
    ]),
    test('splatting object literal (missing keys)', [
      base
        .addCode(`
<ExactComponent {...{a: 'a', b: 1}} />;
        `)
        .newErrors(`
test.js:11
 11:         <ExactComponent {...{a: 'a', b: 1}} />;
              ^^^^^^^^^^^^^^ Cannot create \`ExactComponent\` element because property \`c\` is missing in props [1] but exists in \`ExactProps\` [2].
  References:
   11:         <ExactComponent {...{a: 'a', b: 1}} />;
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
    7:     const ExactComponent: React.ComponentType<ExactProps> = $unimplemented;
                                                     ^^^^^^^^^^ [2]
        `),
    ]),
    test('splatting object literal (extra keys)', [
      base
        .addCode(`
<ExactComponent {...{a: 'a', b: 1, c: false, d: 'extra!'}} />;
        `)
        .newErrors(`
test.js:11
 11:         <ExactComponent {...{a: 'a', b: 1, c: false, d: 'extra!'}} />;
              ^^^^^^^^^^^^^^ Cannot create \`ExactComponent\` element because property \`d\` is missing in \`ExactProps\` [1] but exists in props [2].
  References:
    7:     const ExactComponent: React.ComponentType<ExactProps> = $unimplemented;
                                                     ^^^^^^^^^^ [1]
   11:         <ExactComponent {...{a: 'a', b: 1, c: false, d: 'extra!'}} />;
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ [2]
        `),
    ]),
    test('splatting object literal (missing and extra keys)', [
      base
        .addCode(`
<ExactComponent {...{a: 'a', b: 1, d: 'extra!'}} />;
        `)
        .newErrors(`
test.js:11        11:         <ExactComponent {...{a: 'a', b: 1, d: 'extra!'}} />;
              ^^^^^^^^^^^^^^ Cannot create \`ExactComponent\` element because property \`c\` is missing in props [1] but exists in \`ExactProps\` [2].
  References:
   11:         <ExactComponent {...{a: 'a', b: 1, d: 'extra!'}} />;
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
    7:     const ExactComponent: React.ComponentType<ExactProps> = $unimplemented;
                                                     ^^^^^^^^^^ [2]
test.js:11
 11:         <ExactComponent {...{a: 'a', b: 1, d: 'extra!'}} />;
              ^^^^^^^^^^^^^^ Cannot create \`ExactComponent\` element because property \`d\` is missing in \`ExactProps\` [1] but exists in props [2].
  References:
    7:     const ExactComponent: React.ComponentType<ExactProps> = $unimplemented;
                                                     ^^^^^^^^^^ [1]
   11:         <ExactComponent {...{a: 'a', b: 1, d: 'extra!'}} />;
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ [2]
        `),
    ]),
    test('splatting a variable (correct)', [
      base
        .addCode(`
const props: ExactProps = $unimplemented;
<ExactComponent {...props} />;
        `)
        .noNewErrors(),
    ]),
    test('splatting a variable (correct, structural match)', [
      base
        .addCode(`
const props: {|a: string, b: number, c: boolean|} = $unimplemented;
<ExactComponent {...props} />;
        `)
        .noNewErrors(),
    ]),
    test('splatting a variable (inexact type)', [
      base
        .addCode(`
const props: {a: string, b: number, c: boolean} = $unimplemented;
<ExactComponent {...props} />;
        `)
        .newErrors(`
test.js:12
 12: <ExactComponent {...props} />;
      ^^^^^^^^^^^^^^ Cannot create \`ExactComponent\` element because inexact props [1] is incompatible with exact \`ExactProps\` [2].
  References:
   12: <ExactComponent {...props} />;
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
    7:     const ExactComponent: React.ComponentType<ExactProps> = $unimplemented;
                                                     ^^^^^^^^^^ [2]

        `),
    ]),
    test('splatting a variable (inferred type)', [
      base
        .addCode(`
const props = {a: 'a', b: 1, c: false};
<ExactComponent {...props} />;
        `)
        .noNewErrors(),
    ]),
    test('splatting a variable (inferred type, missing keys)', [
      base
        .addCode(`
const props = {a: 'a', b: 1};
<ExactComponent {...props} />;
        `)
        .newErrors(`
test.js:12
 12: <ExactComponent {...props} />;
      ^^^^^^^^^^^^^^ Cannot create \`ExactComponent\` element because property \`c\` is missing in props [1] but exists in \`ExactProps\` [2].
  References:
   12: <ExactComponent {...props} />;
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
    7:     const ExactComponent: React.ComponentType<ExactProps> = $unimplemented;
                                                      ^^^^^^^^^^ [2]

        `),
    ]),
    test('mixing splat with literal keys (correct)', [
      base
        .addCode(`
const props = {a: 'a', b: 1};
<ExactComponent {...props} c />;
<ExactComponent c {...props} />;
        `)
        .noNewErrors(),
    ]),
    // TODO Respect ordering of splat within JSX attributes
    //     test('mixing splat with literal keys (correct override)', [
    //       base
    //         .addCode(`
    // const props = {a: 'a', b: 1};
    // <ExactComponent c="wrong" {...props} c />;
    // <ExactComponent b="wrong" {...props} c />;
    //         `)
    //         .noNewErrors(),
    //     ]),
    test('mixing splat with literal keys (incorrect override)', [
      base
        .addCode(`
const props = {a: 'a', b: 1};
<ExactComponent {...props} c={3} />;
        `)
        .newErrors(`
test.js:12
 12: <ExactComponent {...props} c={3} />;
                                   ^ Cannot create \`ExactComponent\` element because number [1] is incompatible with boolean [2] in property \`c\`.
  References:
   12: <ExactComponent {...props} c={3} />;
                                     ^ [1]
    6:     type ExactProps = {|a: string, b: number, c: boolean|};
                                                        ^^^^^^^ [2]
        `),
    ]),
    test('multiple splats', [
      base
        .addCode(`
const props1 = {a: 'a'};
const props2 = {b: 1};
<ExactComponent {...props1} {...props2} c />;
        `)
        .noNewErrors(),
    ]),
  ];
});

