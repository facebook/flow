/*
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    ideStartAndConnect,
    ideRequestAndWaitUntilResponse,
    ideNotification,
    addFile,
    modifyFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('textDocument/publishDiagnostics #1', [
      ideStartAndConnect(),
      addFile('witherrors1.js')
        // Flow may send multiple publishDiagnostics when reporting partial
        // progress and then complete results, e.g. an empty publishDiagnostics
        // followed by the final results, or a partial publishDiagnostics followed
        // by the final results, or the full results twice (in case the partial
        // results happened to be identical), or just the full results once.
        // Therefore: when we wait in a step to get publishDiagnostics, it's
        // racey as to whether we'll get one or more publishDiagnostics in the step.
        // To be robust against races: we'll wait up to 9s to get the
        // expected publishDiagnostic, then verify that this expected publishDiagnostic
        // was sent at least once, and ignore any additional publishDiagnostics.
        .waitUntilIDEMessage(
          9000,
          'textDocument/publishDiagnostics{Cannot return}',
        )
        .verifyAllIDEMessagesInStep(
          [
            'textDocument/publishDiagnostics{"Cannot return `23` because  number [1] is incompatible with  string [2].","message":"[1] number","message":"[2] string"}',
          ],
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
    ]),

    test('textDocument/publishDiagnostics #2', [
      ideStartAndConnect(),
      addFile('witherrors2.js')
        .waitUntilIDEMessage(
          9000,
          'textDocument/publishDiagnostics{is not an instance type}',
        )
        .verifyAllIDEMessagesInStep(
          [
            'textDocument/publishDiagnostics{"`H` [1] is not an instance type.","message":"[1] `H`"}',
          ],
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
    ]),

    test('textDocument/publishDiagnostics clears errors', [
      ideStartAndConnect(),
      addFile('witherrors1.js')
        .waitUntilIDEMessage(
          9000,
          'textDocument/publishDiagnostics{Cannot return}',
        )
        .verifyAllIDEMessagesInStep(
          [
            'textDocument/publishDiagnostics{"Cannot return `23` because  number [1] is incompatible with  string [2].","message":"[1] number","message":"[2] string"}',
          ],
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
      modifyFile('witherrors1.js', 'return 23;', 'return "";')
        .waitUntilIDEMessage(
          9000,
          'textDocument/publishDiagnostics{"diagnostics":[]}',
        )
        .verifyAllIDEMessagesInStep(
          ['textDocument/publishDiagnostics{"diagnostics":[]}'],
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
    ]),

    test('live diagnostics', [
      ideStartAndConnect(),
      // Open a document with errors. We should get a live syntax error immediately.
      ideNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL_SLASH>syntaxError1.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow
function fred(): number {return 1+;}
`,
        },
      })
        .waitUntilIDEMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllIDEMessagesInStep(
          ['textDocument/publishDiagnostics{Unexpected token}'],
          ['window/showStatus'],
        ),
      // Edit it fix the problem. The live syntax error should be dismissed immediately.
      ideNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL_SLASH>syntaxError1.js',
          version: 2,
        },
        contentChanges: [
          {
            text: `// @flow
function fred(): number {return 1+2;}
`,
          },
        ],
      })
        .waitUntilIDEMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllIDEMessagesInStep(
          ['textDocument/publishDiagnostics{"diagnostics":[]}'],
          [],
        ),
      // Make another change that doesn't introduce errors. We should get no reports.
      ideNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL_SLASH>syntaxError1.js',
          version: 2,
        },
        contentChanges: [
          {
            text: `// @flow
  function fred(): number {return 1+3;}
  `,
          },
        ],
      })
        .sleep(1000)
        .verifyAllIDEMessagesInStep([], []),
      // Make a change that introduces the error. We should get a report immediately.
      ideNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL_SLASH>syntaxError1.js',
          version: 3,
        },
        contentChanges: [
          {
            text: `// @flow
  function fred(): number {return 1+;}
  `,
          },
        ],
      })
        .waitUntilIDEMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllIDEMessagesInStep(
          ['textDocument/publishDiagnostics{Unexpected token}'],
          [],
        ),
      // Close the file. The live error should go away.
      ideNotification('textDocument/didClose', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL_SLASH>syntaxError1.js',
          version: 3,
        },
      })
        .waitUntilIDEMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllIDEMessagesInStep(
          ['textDocument/publishDiagnostics{"diagnostics":[]}'],
          [],
        ),
    ]),
  ],
);
