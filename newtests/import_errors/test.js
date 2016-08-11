/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('Named import of default-only module', [
    addFile('exporter.js'),
    addFile('importer.js')
      .newErrors(
        `
          importer.js:2
            2: import {a} from "./exporter";
                       ^ Named import from module \`./exporter\`. This module only has a default export. Did you mean \`import a from ...\`?
        `,
      ),
  ]),
]);
