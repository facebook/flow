/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

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
    `).noNewErrors(''),

    addCode(`
      // $FlowFixMe
      ('a': number);
    `).newErrors(`
      test.js:10
       10:       ('a': number);
                  ^^^ string. This type is incompatible with
       10:       ('a': number);
                       ^^^^^^ number
    `),
  ]).flowConfig('_flowconfig_custom_comment'),
]);
