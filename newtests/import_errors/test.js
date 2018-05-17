/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */


import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('Named import of default-only module', [
    addFile('exporter.js'),
    addFile('importer.js')
      .newErrors(
        `
          importer.js:2
            2: import {a} from "./exporter";
                       ^ Cannot import \`a\` because there is no \`a\` export in \`./exporter\`. Did you mean \`import a from "..."\`?
        `,
      ),
  ]),
]);
