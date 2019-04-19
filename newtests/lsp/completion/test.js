/*
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    ideStartAndConnect,
    ideStart,
    ideRequest,
    lspInitializeParams,
    ideRequestAndWaitUntilResponse,
    addFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('textDocument/completion', [
      addFile('completion.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>completion.js'},
        position: {line: 10, character: 15}, // statement position
      }).verifyAllIDEMessagesInStep(
        [
          (() => {
            const expectedResponse = {
              isIncomplete: false,
              items: [
                {
                  label: 'x',
                  kind: 6,
                  detail: 'number',
                  inlineDetail: 'number',
                  insertTextFormat: 1,
                },
                {
                  label: 'fred',
                  kind: 3,
                  detail: '(a: number, b: string) => number',
                  inlineDetail: '(a: number, b: string)',
                  itemType: 'number',
                  insertTextFormat: 1,
                },
                {
                  label: 'exports',
                  kind: 6,
                  detail: '{||}',
                  inlineDetail: '{||}',
                  insertTextFormat: 1,
                },
                {
                  label: 'b',
                  kind: 6,
                  detail: 'string',
                  inlineDetail: 'string',
                  insertTextFormat: 1,
                },
                {
                  label: 'a',
                  kind: 6,
                  detail: 'number',
                  inlineDetail: 'number',
                  insertTextFormat: 1,
                },
                {
                  label: 'this',
                  kind: 6,
                  detail: 'empty',
                  inlineDetail: 'empty',
                  insertTextFormat: 1,
                },
                {
                  label: 'super',
                  kind: 6,
                  detail: 'typeof Object.prototype',
                  inlineDetail: 'typeof Object.prototype',
                  insertTextFormat: 1,
                },
              ],
            };
            return `textDocument/completion${JSON.stringify(expectedResponse)}`;
          })(),
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/completion', [
      addFile('kind.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>kind.js'},
        position: {line: 13, character: 15},
        context: {triggerKind: 1},
      }).verifyAllIDEMessagesInStep(
        [
          (() => {
            const expectedResponse = {
              isIncomplete: false,
              items: [
                {
                  label: 'x',
                  kind: 6,
                  detail: 'number',
                  inlineDetail: 'number',
                  insertTextFormat: 1,
                },
                {
                  label: 'foo',
                  kind: 3,
                  detail: '() => void',
                  inlineDetail: '()',
                  itemType: 'void',
                  insertTextFormat: 1,
                },
                {
                  label: 'exports',
                  kind: 6,
                  detail: '{||}',
                  inlineDetail: '{||}',
                  insertTextFormat: 1,
                },
                {
                  label: 'anInterface',
                  kind: 8,
                  detail: 'interface anInterface',
                  inlineDetail: 'interface anInterface',
                  insertTextFormat: 1,
                },
                {
                  label: 'aUnion',
                  kind: 6,
                  detail: 'number',
                  inlineDetail: 'number',
                  insertTextFormat: 1,
                },
                {
                  label: 'aNumber',
                  kind: 6,
                  detail: 'number',
                  inlineDetail: 'number',
                  insertTextFormat: 1,
                },
                {
                  label: 'aFunction',
                  kind: 3,
                  detail: '() => null',
                  inlineDetail: '()',
                  itemType: 'null',
                  insertTextFormat: 1,
                },
                {
                  label: 'aClass',
                  kind: 7,
                  detail: 'class aClass',
                  inlineDetail: 'class aClass',
                  insertTextFormat: 1,
                },
                {
                  label: 'this',
                  kind: 6,
                  detail: 'empty',
                  inlineDetail: 'empty',
                  insertTextFormat: 1,
                },
                {
                  label: 'super',
                  kind: 6,
                  detail: 'typeof Object.prototype',
                  inlineDetail: 'typeof Object.prototype',
                  insertTextFormat: 1,
                },
              ],
            };
            return `textDocument/completion${JSON.stringify(expectedResponse)}`;
          })(),
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/completion', [
      addFile('params.js'),
      ideStart({mode: 'lsp', needsFlowServer: true}),
      ideRequest('initialize', {
        ...lspInitializeParams,
        capabilities: {
          ...lspInitializeParams.capabilities,
          textDocument: {
            ...lspInitializeParams.capabilities.textDocument,
            completion: {
              completionItem: {
                // snippet support needs to be enabled.
                snippetSupport: true
              }
            }
          }
        }
      }).waitUntilIDEMessage(30000, 'initialize'),
      ideRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>params.js'},
        position: {line: 9, character: 15},
        context: { triggerKind: 1 }
      }).verifyAllIDEMessagesInStep(
        [
          (() => {
            const expectedResponse = {
              isIncomplete: false,
              items: [
                {
                  label: 'x',
                  kind: 6,
                  detail: 'number',
                  inlineDetail: 'number',
                  insertTextFormat: 1
                },
                {
                  label: 'foo',
                  kind: 3,
                  detail: '() => void',
                  inlineDetail: '()',
                  itemType: 'void',
                  insertTextFormat: 2,
                  textEdit: {
                    range: {
                      start: { line: 9, character: 14 },
                      end: { line: 9, character: 16 }
                    },
                    newText: 'foo()'
                  }
                },
                {
                  label: 'exports',
                  kind: 6,
                  detail: '{||}',
                  inlineDetail: '{||}',
                  insertTextFormat: 1
                },
                {
                  label: 'aFunction',
                  kind: 3,
                  detail: '(arg1: number, arg2: string) => null',
                  inlineDetail: '(arg1: number, arg2: string)',
                  itemType: 'null',
                  insertTextFormat: 2,
                  textEdit: {
                    range: {
                      start: { line: 9, character: 14 },
                      end: { line: 9, character: 16 }
                    },
                    newText: 'aFunction(${1:arg1}, ${2:arg2})'
                  }
                },
                {
                  label: 'this',
                  kind: 6,
                  detail: 'empty',
                  inlineDetail: 'empty',
                  insertTextFormat: 1
                },
                {
                  label: 'super',
                  kind: 6,
                  detail: 'typeof Object.prototype',
                  inlineDetail: 'typeof Object.prototype',
                  insertTextFormat: 1
                }
              ]
            };
            return `textDocument/completion${JSON.stringify(expectedResponse)}`
          })()
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ]
);
