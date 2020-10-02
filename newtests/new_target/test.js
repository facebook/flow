/*
 * @flow
 */


import type Suite from "flow-dev-tools/src/test/Suite.js";
import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default (suite(({addFile, addFiles, addCode}) => [
  test('new.target is not supported yet', [
    addCode(`
      function x() { new.target(); }
    `).newErrors(
        `
          test.js:4
            4:       function x() { new.target(); }
                                    ^^^^^^^^^^ Not supported. [unsupported-syntax]
        `,
      ),
  ]),

  test('new.target can be suppressed', [
    addCode(`
      // $FlowFixMe
      function x() { new.target; }
    `).noNewErrors(),
  ]),
]): Suite);
