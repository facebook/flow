/*
 * @flow
 */


import type {Suite} from "flow-dev-tools/src/test/Suite";
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(({addFile, addFiles, addCode}) => [
  test('Make sure the server does not die when a parse error is introduced', [
    addCode('var x: string = "hello";')
      .noNewErrors(),
    addCode('I am a parse error')
      .newErrors(
        `
          test.js:5
            5: I am a parse error
                 ^^ Unexpected identifier, expected the end of an expression statement (\`;\`)
        `,
      ),
  ]),
]): Suite);
