/*
 * @flow
 * @format
 */

import type Suite from 'flow-dev-tools/src/test/Suite.js';
import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default (suite(
  ({
    lspStartAndConnect,
    lspStart,
    lspRequest,
    lspInitializeParams,
    lspRequestAndWaitUntilResponse,
    addFiles,
    addCode,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('textDocument/completion with autoimports', [
      addFiles('foo.js', 'bar.js', 'foobar.js', 'lib/builtins.js'),
      addCode(`f`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 1},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/completion',
            JSON.stringify({
              isIncomplete: false,
              items: [
                {
                  label: 'undefined',
                  kind: 6,
                  detail: 'undefined',
                  sortText: '00000000000000000101',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 2, character: 0},
                      end: {line: 2, character: 1},
                    },
                    newText: 'undefined',
                  },
                },
                {
                  label: 'Function',
                  kind: 6,
                  detail: 'Function',
                  sortText: '00000000000000000101',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 2, character: 0},
                      end: {line: 2, character: 1},
                    },
                    newText: 'Function',
                  },
                },
                {
                  label: 'foobar',
                  kind: 6,
                  detail: 'foobar',
                  documentation: {
                    kind: 'markdown',
                    value: 'Import default from ./foobar',
                  },
                  sortText: '00000000000000000100',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 2, character: 0},
                      end: {line: 2, character: 1},
                    },
                    newText: 'foobar',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {line: 2, character: 0},
                        end: {line: 2, character: 0},
                      },
                      newText: 'import foobar from "./foobar";\n\n',
                    },
                  ],
                },
                {
                  label: 'Foo',
                  kind: 6,
                  detail: 'Foo',
                  documentation: {kind: 'markdown', value: 'Import from ./foo'},
                  sortText: '00000000000000000100',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 2, character: 0},
                      end: {line: 2, character: 1},
                    },
                    newText: 'Foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {line: 2, character: 0},
                        end: {line: 2, character: 0},
                      },
                      newText: 'import {Foo} from "./foo";\n\n',
                    },
                  ],
                },
                {
                  label: 'foo',
                  kind: 6,
                  detail: 'foo',
                  documentation: {
                    kind: 'markdown',
                    value: 'Import * from ./foo',
                  },
                  sortText: '00000000000000000100',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 2, character: 0},
                      end: {line: 2, character: 1},
                    },
                    newText: 'foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {line: 2, character: 0},
                        end: {line: 2, character: 0},
                      },
                      newText: 'import * as foo from "./foo";\n\n',
                    },
                  ],
                },
                {
                  label: 'foo',
                  kind: 6,
                  detail: 'foo',
                  documentation: {
                    kind: 'markdown',
                    value: 'Import default from ./foo',
                  },
                  sortText: '00000000000000000100',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 2, character: 0},
                      end: {line: 2, character: 1},
                    },
                    newText: 'foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {line: 2, character: 0},
                        end: {line: 2, character: 0},
                      },
                      newText: 'import foo from "./foo";\n\n',
                    },
                  ],
                },
                {
                  label: 'foo',
                  kind: 6,
                  detail: 'foo',
                  documentation: {kind: 'markdown', value: 'Import from LibA'},
                  sortText: '00000000000000000100',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 2, character: 0},
                      end: {line: 2, character: 1},
                    },
                    newText: 'foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {line: 2, character: 0},
                        end: {line: 2, character: 0},
                      },
                      newText: 'import {foo} from "LibA";\n\n',
                    },
                  ],
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]).flowConfig('_flowconfig_autoimports'),
  ],
): Suite);
