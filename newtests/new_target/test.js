/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('new.target is not supported yet', [
    addCode(`
      function x() { new.target(); }
    `).newErrors(
        `
          test.js:4
            4:       function x() { new.target(); }
                                    ^^^^^^^^^^ not (sup)ported
        `,
      ),
  ]),

  test('new.target can be suppressed', [
    addCode(`
      // $FlowFixMe
      function x() { new.target; }
    `).noNewErrors(),
  ]),
]);
