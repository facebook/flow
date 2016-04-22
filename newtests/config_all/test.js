/* @flow */

import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile}) => [
  test('all=true', [
    addFile('no_at_flow.js')
      .newErrors(
`no_at_flow.js:1
  1: var x: number = "not a number";
                     ^^^^^^^^^^^^^^ string. This type is incompatible with
  1: var x: number = "not a number";
            ^^^^^^ number`,
)
      .because('We read files even if they are missing @flow'),
  ]).flowConfig('flowconfig_all_true'),

  test('all=false', [
    addFile('no_at_flow.js')
      .noNewErrors()
      .because('We ignore files without @flow'),
  ]).flowConfig('flowconfig_all_false'),
]);
