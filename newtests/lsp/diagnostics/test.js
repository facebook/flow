/*
 * @flow
 * @format
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    lspNotification,
    addFile,
    addFiles,
    modifyFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('textDocument/publishDiagnostics #1', [
      lspStartAndConnect(),
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
        .waitUntilLSPMessage(
          9000,
          'textDocument/publishDiagnostics{Cannot return}',
        )
        .verifyAllLSPMessagesInStep(
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
      lspStartAndConnect(),
      addFile('witherrors2.js')
        .waitUntilLSPMessage(
          9000,
          'textDocument/publishDiagnostics{Cannot extend}',
        )
        .verifyAllLSPMessagesInStep(
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
      lspStartAndConnect(),
      addFile('witherrors1.js')
        .waitUntilLSPMessage(
          9000,
          'textDocument/publishDiagnostics{Cannot return}',
        )
        .verifyAllLSPMessagesInStep(
          [
            'textDocument/publishDiagnostics{"Cannot return `23` because  number [1] is incompatible with  string [2].","message":"[1] number","message":"[2] string"}',
          ],
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
      modifyFile('witherrors1.js', 'return 23;', 'return "";')
        .waitUntilLSPMessage(
          9000,
          'textDocument/publishDiagnostics{"diagnostics":[]}',
        )
        .verifyAllLSPMessagesInStep(
          ['textDocument/publishDiagnostics{"diagnostics":[]}'],
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
    ]),

    test('live parse diagnostics', [
      lspStartAndConnect(),
      // Open a document with errors. We should get a live syntax error immediately.
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/syntaxError1.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow
    function fred(): number {return 1+;}
    `,
        },
      })
        .waitUntilLSPMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllLSPMessagesInStep(
          ['textDocument/publishDiagnostics{Unexpected token}'],
          ['window/showStatus'],
        ),
      // Edit it fix the problem. The live syntax error should be dismissed immediately.
      lspNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/syntaxError1.js',
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
        .waitUntilLSPMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllLSPMessagesInStep(
          ['textDocument/publishDiagnostics{"diagnostics":[]}'],
          [],
        ),
      // Make another change that doesn't introduce errors. We should get no reports.
      lspNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/syntaxError1.js',
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
        .verifyAllLSPMessagesInStep([], []),
      // Make a change that introduces the error. We should get a report immediately.
      lspNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/syntaxError1.js',
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
        .waitUntilLSPMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllLSPMessagesInStep(
          ['textDocument/publishDiagnostics{Unexpected token}'],
          [],
        ),
      // Close the file. The live error should go away.
      lspNotification('textDocument/didClose', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/syntaxError1.js',
          version: 3,
        },
      })
        .waitUntilLSPMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllLSPMessagesInStep(
          ['textDocument/publishDiagnostics{"diagnostics":[]}'],
          [],
        ),
    ]),
    test('live non-parse diagnostics', [
      lspStartAndConnect(),
      // Open a document with no errors. We should not see errors
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/typeError1.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow`,
        },
      })
        .sleep(1000)
        .verifyAllLSPMessagesInStep([], ['window/showStatus']),
      // Edit it and add a type error. We should see the error.
      lspNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/typeError1.js',
          version: 2,
        },
        contentChanges: [
          {
            text: `// @flow
    let x: string = 123;
    `,
          },
        ],
      })
        .waitUntilLSPMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllLSPMessagesInStep(
          [
            'textDocument/publishDiagnostics{Cannot assign `123` to `x` because  number [1] is incompatible with  string [2].","message":"[1] number","message":"[2] string"}',
          ],
          ['window/showStatus'],
        ),
      // Edit it fix the problem. The live type error should be dismissed.
      lspNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/typeError1.js',
          version: 3,
        },
        contentChanges: [
          {
            text: `// @flow
    let x: string = "hello";
    `,
          },
        ],
      })
        .waitUntilLSPMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllLSPMessagesInStep(
          ['textDocument/publishDiagnostics{"diagnostics":[]}'],
          ['window/showStatus'],
        ),
      // Make another change that doesn't introduce errors. We should get no reports.
      lspNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/typeError1.js',
          version: 4,
        },
        contentChanges: [
          {
            text: `// @flow
      let x: string = "hello there";
      `,
          },
        ],
      })
        .sleep(1000)
        .verifyAllLSPMessagesInStep([], ['window/showStatus']),
      // Make a change that introduces the error. We should get a report immediately.
      lspNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/typeError1.js',
          version: 5,
        },
        contentChanges: [
          {
            text: `// @flow
      let x: string = "hello there";
      let y: string = 123;
      `,
          },
        ],
      })
        .waitUntilLSPMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllLSPMessagesInStep(
          [
            'textDocument/publishDiagnostics{Cannot assign `123` to `y` because  number [1] is incompatible with  string [2].","message":"[1] number","message":"[2] string"}',
          ],
          ['window/showStatus'],
        ),
      // Close the file. The live error should go away.
      lspNotification('textDocument/didClose', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/typeError1.js',
          version: 6,
        },
      })
        .waitUntilLSPMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllLSPMessagesInStep(
          ['textDocument/publishDiagnostics{"diagnostics":[]}'],
          ['window/showStatus'],
        ),
    ]),
    test('live non-parse diagnostics with unchecked dependencies', [
      addFile('dependency.js'),
      lspStartAndConnect(),
      // Open a document with errors. We should immediately see the live non-parse errors
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/typeError1.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow
    let dependency = require('./dependency');
    let x: string = 123;
    `,
        },
      })
        .waitUntilLSPMessage(
          9000,
          'textDocument/publishDiagnostics{Cannot assign `123` to `x`}',
        )
        .verifyAllLSPMessagesInStep(
          [
            'textDocument/publishDiagnostics{Cannot assign `123` to `x` because  number [1] is incompatible with  string [2].","message":"[1] number","message":"[2] string"}',
          ],
          ['window/showStatus', 'textDocument/publishDiagnostics'],
        ),
    ]).flowConfig('_flowconfig_lazy'),
    test('live non-parse diagnostics can be disabled in .flowconfig', [
      lspStartAndConnect(),
      // Open a document with no errors. We should not see errors
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/typeError1.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow`,
        },
      })
        .sleep(1000)
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation]),
      // Edit it and add a type error. We won't see the error since
      // experimental.disable_live_non_parse_errors=true
      // is set in the .flowconfig
      lspNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/typeError1.js',
          version: 2,
        },
        contentChanges: [
          {
            text: `// @flow
    let x: string = 123;
    `,
          },
        ],
      })
        .sleep(1000)
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation]),
    ]).flowConfig('_flowconfig_disable_live_non_parse_errors'),
    test('live non-parse diagnostics respect missing @flow pragma', [
      lspStartAndConnect(),
      // Open a document with no errors. We should not see errors
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/typeError1.js',
          languageId: 'javascript',
          version: 1,
          text: `const bad = require("./bad");`,
        },
      })
        .sleep(1000)
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation]),
      // Edit it and add a type error. We won't see the error since
      // experimental.disable_live_non_parse_errors=true
      // is set in the .flowconfig
      lspNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/typeError1.js',
          version: 2,
        },
        contentChanges: [
          {
            text: `const bad = require("./bad");
    let x: string = 123;
    `,
          },
        ],
      })
        .sleep(1000)
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation]),
    ]),
    test(
      'live non-parse diagnostics ignores missing @flow pragma with all=true',
      [
        lspStartAndConnect(),
        // Open a document with no errors. We should not see errors
        lspNotification('textDocument/didOpen', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/typeError1.js',
            languageId: 'javascript',
            version: 1,
            text: `const bad = require("./bad");`,
          },
        })
          .waitUntilLSPMessage(
            9000,
            'textDocument/publishDiagnostics{Cannot resolve module `./bad`.}',
          )
          .verifyAllLSPMessagesInStep(
            ['textDocument/publishDiagnostics{Cannot resolve module `./bad`.}'],
            [
              'textDocument/publishDiagnostics',
              ...lspIgnoreStatusAndCancellation,
            ],
          ),
        // Edit it and add a type error. We won't see the error since
        // experimental.disable_live_non_parse_errors=true
        // is set in the .flowconfig
        lspNotification('textDocument/didChange', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/typeError1.js',
            version: 2,
          },
          contentChanges: [
            {
              text: `let x: string = 123;
    `,
            },
          ],
        })
          .waitUntilLSPMessage(
            9000,
            'textDocument/publishDiagnostics{Cannot assign `123` to `x` because  number [1] is incompatible with  string [2].}',
          )
          .verifyAllLSPMessagesInStep(
            [
              'textDocument/publishDiagnostics{Cannot assign `123` to `x` because  number [1] is incompatible with  string [2].}',
            ],
            [
              'textDocument/publishDiagnostics',
              ...lspIgnoreStatusAndCancellation,
            ],
          ),
      ],
    ).flowConfig('_flowconfig_all_set_to_true'),
    test('live non-parse diagnostics ignores ignored file', [
      lspStartAndConnect(),
      // Open an ignored document with errors. We should not get errors
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/ignoreme.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow
          let x: string = 123;
          `,
        },
      })
        .sleep(1000)
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation]),
      // Modified an ignored document with errors. We should not get errors
      lspNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/ignoreme.js',
          version: 2,
        },
        contentChanges: [
          {
            text: `// @flow
          let x: boolean = 123;
          `,
          },
        ],
      })
        .sleep(1000)
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation]),
    ]).flowConfig('_flowconfig_with_ignores'),
    test('live non-parse diagnostics ignores non-flow files', [
      lspStartAndConnect(),
      // Open a document with the wrong extension
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/foo.php',
          languageId: 'javascript',
          version: 1,
          text: `// @flow
          let x: string = 123;
          `,
        },
      })
        .sleep(1000)
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation]),
      addFile('witherrors1.js', 'directory.js/with_errors.js.ignored')
        .sleep(1000)
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation]),
      // Open a "document" which actually is a directory
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/directory.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow
          let x: string = 123;
          `,
        },
      })
        .sleep(1000)
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation]),
    ]),
    test('live non-parse diagnostics resent after recheck', [
      addFile('export_number.js', 'importme.js'),
      lspStartAndConnect(),
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/foo.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow
          import value from './importme';
          (value: boolean); // This will error
          `,
        },
      })
        .waitUntilLSPMessage(
          9000,
          'textDocument/publishDiagnostics{Cannot cast `value` to boolean because  number [1] is incompatible with  boolean [2].}',
        )
        .verifyAllLSPMessagesInStep(
          [
            'textDocument/publishDiagnostics{Cannot cast `value` to boolean because  number [1] is incompatible with  boolean [2].}',
          ],
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
      // Changing importme.js will trigger a recheck, which will resend the live non-parse errors
      addFile('export_string.js', 'importme.js')
        .waitUntilLSPMessage(
          9000,
          'textDocument/publishDiagnostics{Cannot cast `value` to boolean because  string [1] is incompatible with  boolean [2].}',
        )
        .verifyAllLSPMessagesInStep(
          [
            'textDocument/publishDiagnostics{Cannot cast `value` to boolean because  string [1] is incompatible with  boolean [2].}',
          ],
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
    ]),
    test('Errors with Loc.none', [
      lspStartAndConnect(),
      addFiles('empty.js', 'importsFakeSymbol.js').waitUntilLSPMessage(
        9000,
        (() => {
          const expectedMessage = {
            uri: '<PLACEHOLDER_PROJECT_URL>/importsFakeSymbol.js',
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
                      uri: '<PLACEHOLDER_PROJECT_URL>/empty.js',
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
                      uri: '<PLACEHOLDER_PROJECT_URL>/empty.js',
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
