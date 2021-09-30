/*
 * @flow
 */


import type {Suite} from "flow-dev-tools/src/test/Suite";
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(({addFile, addFiles, addCode}) => [
  test('Named import of default-only module', [
    addFile('exporter.js'),
    addFile('importer.js')
      .newErrors(
        `
          importer.js:2
            2: import {a} from "./exporter";
                       ^ Cannot import \`a\` because there is no \`a\` export in \`./exporter\`. Did you mean \`import a from "..."\`? [missing-export]
        `,
      ),
  ]),
]): Suite);
