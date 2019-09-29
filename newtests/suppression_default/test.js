/*
 * @flow
 */


import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('No custom suppress_comments', [
    addCode(`
      // $FlowFixMe
      ("a": number);
    `).noNewErrors(),
  ]),

  test('Custom suppress_comments', [
    addCode(`
      // $TestSuppression
      ('a': number);
    `).noNewErrors(),

    addCode(`
      // $FlowFixMe
      ('a': number);
    `).newErrors(
        `
          test.js:10
           10:       ('a': number);
                      ^^^ Cannot cast \`'a'\` to number because string [1] is incompatible with number [2].
            References:
             10:       ('a': number);
                        ^^^ [1]
             10:       ('a': number);
                             ^^^^^^ [2]
        `,
      ),
  ]).flowConfig('_flowconfig_custom_comment'),
]);
