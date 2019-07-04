/*
 * @flow
 * @format
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    ideStartAndConnect,
    ideRequestAndWaitUntilResponse,
    ideNotification,
    addFile,
    addFiles,
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
          'textDocument/publishDiagnostics{Cannot extend}',
        )
        .verifyAllIDEMessagesInStep(
          [
            'textDocument/publishDiagnostics{"Cannot extend  `H` [1] with `I` because  `H` [1] is not inheritable.","message":"[1] `H`"}',
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
    test('pseudo parse errors', [
      ideStartAndConnect(),
      addFile('pseudo_parse_error.js')
        .waitUntilIDEMessage(
          9000,
          'textDocument/publishDiagnostics{Cannot return}',
        )
        .verifyAllIDEMessagesInStep(
          [
            'textDocument/publishDiagnostics{"Flow does not yet support method or property calls in optional chains."}',
          ],
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        )
        .newErrors(
          `
                pseudo_parse_error.js:6
                  6: obj?.foo(); // Error
                     ^^^^^^^^^^ Flow does not yet support method or property calls in optional chains.
              `,
        ),
      ideNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL_SLASH>pseudo_parse_error.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow

const obj = {};
// Flow does not yet support method or property calls in optional chains, so
// this will produce a pseudo parse error
obj?.foo(); // Error
`,
        },
      })
        .waitUntilIDEMessage(
          9000,
          'textDocument/publishDiagnostics{Cannot return}',
        )
        .verifyAllIDEMessagesInStep(
          [
            'textDocument/publishDiagnostics{"Flow does not yet support method or property calls in optional chains."}',
          ],
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
    ]),
    test('Errors with Loc.none', [
      ideStartAndConnect(),
      addFiles('empty.js', 'importsFakeSymbol.js').waitUntilIDEMessage(
        9000,
        (() => {
          const expectedMessage = {
            uri: '<PLACEHOLDER_PROJECT_URL_SLASH>importsFakeSymbol.js',
            diagnostics: [
              {
                range: {
                  start: {
                    line: 2,
                    character: 7,
                  },
                  end: {
                    line: 2,
                    character: 10,
                  },
                },
                severity: 1,
                code: 'InferError',
                source: 'Flow',
                message: 'property `foo` is missing in  exports [1].',
                relatedInformation: [
                  {
                    location: {
                      uri: '<PLACEHOLDER_PROJECT_URL_SLASH>empty.js',
                      range: {
                        start: {
                          line: 0,
                          character: 0,
                        },
                        end: {
                          line: 0,
                          character: 0,
                        },
                      },
                    },
                    message: '[1] exports',
                  },
                ],
                relatedLocations: [
                  {
                    location: {
                      uri: '<PLACEHOLDER_PROJECT_URL_SLASH>empty.js',
                      range: {
                        start: {line: 0, character: 0},
                        end: {line: 0, character: 0},
                      },
                    },
                    message: '[1] exports',
                  },
                ],
              },
            ],
          };
          return `textDocument/publishDiagnostics${JSON.stringify(
            expectedMessage,
          )}`;
        })(),
      ),
    ]),
  ],
);
