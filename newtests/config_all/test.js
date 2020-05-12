/*
 * @flow
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(({addFile}) => [
  test('all=true', [
    addFile('no_at_flow.js')
      .newErrors(
        `
          no_at_flow.js:1
            1: var x: number = "not a number";
                               ^^^^^^^^^^^^^^ Cannot assign \`"not a number"\` to \`x\` because string [1] is incompatible with number [2]. [incompatible-type]
            References:
              1: var x: number = "not a number";
                                 ^^^^^^^^^^^^^^ [1]
              1: var x: number = "not a number";
                        ^^^^^^ [2]
        `,
      )
      .because('We read files even if they are missing @flow'),
  ]).flowConfig('flowconfig_all_true'),

  test('all=false', [
    addFile('no_at_flow.js')
      .noNewErrors()
      .because('We ignore files without @flow'),
  ]).flowConfig('flowconfig_all_false'),
]);
