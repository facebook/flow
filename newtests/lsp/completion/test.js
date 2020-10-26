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
    addFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('textDocument/completion', [
      addFile('completion.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/completion.js'},
        position: {line: 10, character: 15}, // statement position
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/completion',
            JSON.stringify({
              isIncomplete: false,
              items: [
                {
                  label: 'this',
                  kind: 6,
                  detail: 'this',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 10, character: 15},
                      end: {line: 10, character: 15},
                    },
                    newText: 'this',
                  },
                },
                {
                  label: 'x',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 10, character: 15},
                      end: {line: 10, character: 15},
                    },
                    newText: 'x',
                  },
                },
                {
                  label: 'fred',
                  kind: 3,
                  detail: '(a: number, b: string) => number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 10, character: 15},
                      end: {line: 10, character: 15},
                    },
                    newText: 'fred',
                  },
                },
                {
                  label: 'b',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 10, character: 15},
                      end: {line: 10, character: 15},
                    },
                    newText: 'b',
                  },
                },
                {
                  label: 'a',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 10, character: 15},
                      end: {line: 10, character: 15},
                    },
                    newText: 'a',
                  },
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/completion', [
      addFile('kind.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/kind.js'},
        position: {line: 13, character: 15},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/completion',
            JSON.stringify({
              isIncomplete: false,
              items: [
                {
                  label: 'this',
                  kind: 6,
                  detail: 'this',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 13, character: 15},
                      end: {line: 13, character: 15},
                    },
                    newText: 'this',
                  },
                },
                {
                  label: 'x',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 13, character: 15},
                      end: {line: 13, character: 15},
                    },
                    newText: 'x',
                  },
                },
                {
                  label: 'foo',
                  kind: 3,
                  detail: '() => void',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 13, character: 15},
                      end: {line: 13, character: 15},
                    },
                    newText: 'foo',
                  },
                },
                {
                  label: 'anInterface',
                  kind: 8,
                  detail: 'interface anInterface',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 13, character: 15},
                      end: {line: 13, character: 15},
                    },
                    newText: 'anInterface',
                  },
                },
                {
                  label: 'aUnion',
                  kind: 13,
                  detail: 'type aUnion = "a" | "b"',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 13, character: 15},
                      end: {line: 13, character: 15},
                    },
                    newText: 'aUnion',
                  },
                },
                {
                  label: 'aNumber',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 13, character: 15},
                      end: {line: 13, character: 15},
                    },
                    newText: 'aNumber',
                  },
                },
                {
                  label: 'aFunction',
                  kind: 3,
                  detail: '() => null',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 13, character: 15},
                      end: {line: 13, character: 15},
                    },
                    newText: 'aFunction',
                  },
                },
                {
                  label: 'aClass',
                  kind: 7,
                  detail: 'class aClass',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 13, character: 15},
                      end: {line: 13, character: 15},
                    },
                    newText: 'aClass',
                  },
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/completion', [
      addFile('params.js'),
      lspStartAndConnect(6000, {
        ...lspInitializeParams,
        capabilities: {
          ...lspInitializeParams.capabilities,
          textDocument: {
            ...lspInitializeParams.capabilities.textDocument,
            completion: {
              completionItem: {
                // snippet support needs to be enabled.
                snippetSupport: true,
              },
            },
          },
        },
      }),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/params.js'},
        position: {line: 9, character: 15},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/completion',
            JSON.stringify({
              isIncomplete: false,
              items: [
                {
                  label: 'this',
                  kind: 6,
                  detail: 'this',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 9, character: 15},
                      end: {line: 9, character: 15},
                    },
                    newText: 'this',
                  },
                },
                {
                  label: 'x',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 9, character: 15},
                      end: {line: 9, character: 15},
                    },
                    newText: 'x',
                  },
                },
                {
                  label: 'foo',
                  kind: 3,
                  detail: '() => void',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 9, character: 15},
                      end: {line: 9, character: 15},
                    },
                    newText: 'foo',
                  },
                },
                {
                  label: 'aFunction',
                  kind: 3,
                  detail: '(arg1: number, arg2: string) => null',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 9, character: 15},
                      end: {line: 9, character: 15},
                    },
                    newText: 'aFunction',
                  },
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/completion triggered by space in jsx', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 12, character: 4},
        context: {triggerKind: 2, triggerCharacter: ' '},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/completion',
            JSON.stringify({
              isIncomplete: false,
              items: [
                {
                  label: 'a',
                  kind: 6,
                  detail: 'number',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 12, character: 4},
                      end: {line: 12, character: 4},
                    },
                    newText: 'a=',
                  },
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/completion triggered by space outside of jsx', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 11, character: 1},
        context: {triggerKind: 2, triggerCharacter: ' '},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/completion',
            JSON.stringify({
              isIncomplete: false,
              items: [],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/completion invoked outside of jsx', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 11, character: 1},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/completion',
            JSON.stringify({
              isIncomplete: false,
              items: [
                {
                  label: 'React',
                  kind: 9,
                  detail: 'module React',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 11, character: 1},
                      end: {line: 11, character: 1},
                    },
                    newText: 'React',
                  },
                },
                {
                  label: 'Props',
                  kind: 13,
                  detail: 'type Props = {a: number, ...}',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 11, character: 1},
                      end: {line: 11, character: 1},
                    },
                    newText: 'Props',
                  },
                },
                {
                  label: 'D',
                  kind: 3,
                  detail: '(props: Props) => void',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 11, character: 1},
                      end: {line: 11, character: 1},
                    },
                    newText: 'D',
                  },
                },
                {
                  label: 'C',
                  kind: 7,
                  detail: 'class C',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 11, character: 1},
                      end: {line: 11, character: 1},
                    },
                    newText: 'C',
                  },
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/completion invoked in jsx', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 12, character: 4},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/completion',
            JSON.stringify({
              isIncomplete: false,
              items: [
                {
                  label: 'a',
                  kind: 6,
                  detail: 'number',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 12, character: 4},
                      end: {line: 12, character: 4},
                    },
                    newText: 'a=',
                  },
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test(
      'textDocument/completion triggered by space in jsx, function component',
      [
        addFile('jsx.js'),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/completion', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
          position: {line: 13, character: 4},
          context: {triggerKind: 2, triggerCharacter: ' '},
        }).verifyAllLSPMessagesInStep(
          [
            [
              'textDocument/completion',
              JSON.stringify({
                isIncomplete: false,
                items: [
                  {
                    label: 'a',
                    kind: 6,
                    detail: 'number',
                    insertTextFormat: 1,
                    textEdit: {
                      range: {
                        start: {line: 13, character: 4},
                        end: {line: 13, character: 4},
                      },
                      newText: 'a=',
                    },
                  },
                ],
              }),
            ],
          ],
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
      ],
    ),
    test('textDocument/completion invoked in jsx, function component', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 13, character: 4},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/completion',
            JSON.stringify({
              isIncomplete: false,
              items: [
                {
                  label: 'a',
                  kind: 6,
                  detail: 'number',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 13, character: 4},
                      end: {line: 13, character: 4},
                    },
                    newText: 'a=',
                  },
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/completion triggered by dot in jsx', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 14, character: 3},
        context: {triggerKind: 2, triggerCharacter: '.'},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/completion',
            JSON.stringify({
              isIncomplete: false,
              items: [
                {
                  label: 'apply',
                  kind: 3,
                  detail: '(thisArg: any, argArray?: any) => any',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'apply',
                  },
                },
                {
                  label: 'arguments',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'arguments',
                  },
                },
                {
                  label: 'bind',
                  kind: 3,
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'bind',
                  },
                },
                {
                  label: 'call',
                  kind: 3,
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'call',
                  },
                },
                {
                  label: 'caller',
                  kind: 13,
                  detail: 'any | null',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'caller',
                  },
                },
                {
                  label: 'childContextTypes',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'childContextTypes',
                  },
                },
                {
                  label: 'contextTypes',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'contextTypes',
                  },
                },
                {
                  label: 'displayName',
                  kind: 13,
                  detail: '(?string) | void',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'displayName',
                  },
                },
                {
                  label: 'hasOwnProperty',
                  kind: 3,
                  detail: '(prop: mixed) => boolean',
                  documentation: {
                    kind: 'markdown',
                    value:
                      'Determines whether an object has a property with the specified name.',
                  },
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'hasOwnProperty',
                  },
                },
                {
                  label: 'isPrototypeOf',
                  kind: 3,
                  detail: '(o: mixed) => boolean',
                  documentation: {
                    kind: 'markdown',
                    value:
                      "Determines whether an object exists in another object's prototype chain.",
                  },
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'isPrototypeOf',
                  },
                },
                {
                  label: 'length',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'length',
                  },
                },
                {
                  label: 'name',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'name',
                  },
                },
                {
                  label: 'propTypes',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'propTypes',
                  },
                },
                {
                  label: 'propertyIsEnumerable',
                  kind: 3,
                  detail: '(prop: mixed) => boolean',
                  documentation: {
                    kind: 'markdown',
                    value:
                      'Determines whether a specified property is enumerable.',
                  },
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'propertyIsEnumerable',
                  },
                },
                {
                  label: 'toLocaleString',
                  kind: 3,
                  detail: '() => string',
                  documentation: {
                    kind: 'markdown',
                    value:
                      'Returns a date converted to a string using the current locale.',
                  },
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'toLocaleString',
                  },
                },
                {
                  label: 'toString',
                  kind: 3,
                  detail: '() => string',
                  documentation: {
                    kind: 'markdown',
                    value: 'Returns a string representation of an object.',
                  },
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'toString',
                  },
                },
                {
                  label: 'valueOf',
                  kind: 3,
                  detail: '() => mixed',
                  documentation: {
                    kind: 'markdown',
                    value:
                      'Returns the primitive value of the specified object.',
                  },
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 14, character: 3},
                      end: {line: 14, character: 3},
                    },
                    newText: 'valueOf',
                  },
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/completion triggered by dot outside jsx', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 15, character: 2},
        context: {triggerKind: 2, triggerCharacter: '.'},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/completion',
            JSON.stringify({
              isIncomplete: false,
              items: [
                {
                  label: 'apply',
                  kind: 3,
                  detail: '(thisArg: any, argArray?: any) => any',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'apply',
                  },
                },
                {
                  label: 'arguments',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'arguments',
                  },
                },
                {
                  label: 'bind',
                  kind: 3,
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'bind',
                  },
                },
                {
                  label: 'call',
                  kind: 3,
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'call',
                  },
                },
                {
                  label: 'caller',
                  kind: 13,
                  detail: 'any | null',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'caller',
                  },
                },
                {
                  label: 'childContextTypes',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'childContextTypes',
                  },
                },
                {
                  label: 'contextTypes',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'contextTypes',
                  },
                },
                {
                  label: 'displayName',
                  kind: 13,
                  detail: '(?string) | void',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'displayName',
                  },
                },
                {
                  label: 'hasOwnProperty',
                  kind: 3,
                  detail: '(prop: mixed) => boolean',
                  documentation: {
                    kind: 'markdown',
                    value:
                      'Determines whether an object has a property with the specified name.',
                  },
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'hasOwnProperty',
                  },
                },
                {
                  label: 'isPrototypeOf',
                  kind: 3,
                  detail: '(o: mixed) => boolean',
                  documentation: {
                    kind: 'markdown',
                    value:
                      "Determines whether an object exists in another object's prototype chain.",
                  },
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'isPrototypeOf',
                  },
                },
                {
                  label: 'length',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'length',
                  },
                },
                {
                  label: 'name',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'name',
                  },
                },
                {
                  label: 'propTypes',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'propTypes',
                  },
                },
                {
                  label: 'propertyIsEnumerable',
                  kind: 3,
                  detail: '(prop: mixed) => boolean',
                  documentation: {
                    kind: 'markdown',
                    value:
                      'Determines whether a specified property is enumerable.',
                  },
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'propertyIsEnumerable',
                  },
                },
                {
                  label: 'toLocaleString',
                  kind: 3,
                  detail: '() => string',
                  documentation: {
                    kind: 'markdown',
                    value:
                      'Returns a date converted to a string using the current locale.',
                  },
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'toLocaleString',
                  },
                },
                {
                  label: 'toString',
                  kind: 3,
                  detail: '() => string',
                  documentation: {
                    kind: 'markdown',
                    value: 'Returns a string representation of an object.',
                  },
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'toString',
                  },
                },
                {
                  label: 'valueOf',
                  kind: 3,
                  detail: '() => mixed',
                  documentation: {
                    kind: 'markdown',
                    value:
                      'Returns the primitive value of the specified object.',
                  },
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 15, character: 2},
                      end: {line: 15, character: 2},
                    },
                    newText: 'valueOf',
                  },
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/completion in an unqualified type annotation', [
      addFile('type-exports.js'),
      addFile('unqualified-type-annotation.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/unqualified-type-annotation.js',
        },
        position: {line: 27, character: 18},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/completion',
            JSON.stringify({
              isIncomplete: false,
              items: [
                {
                  label: 'Typologies',
                  kind: 9,
                  detail: 'module Typologies',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 27, character: 18},
                      end: {line: 27, character: 18},
                    },
                    newText: 'Typologies.',
                  },
                },
                {
                  label: 'Typography',
                  kind: 7,
                  detail: 'class Typewriter',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 27, character: 18},
                      end: {line: 27, character: 18},
                    },
                    newText: 'Typography',
                  },
                },
                {
                  label: 'Typewriter',
                  kind: 7,
                  detail: 'class Typewriter',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 27, character: 18},
                      end: {line: 27, character: 18},
                    },
                    newText: 'Typewriter',
                  },
                },
                {
                  label: 'Types',
                  kind: 9,
                  detail: 'module Types',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 27, character: 18},
                      end: {line: 27, character: 18},
                    },
                    newText: 'Types.',
                  },
                },
                {
                  label: 'Typesafe',
                  kind: 8,
                  detail: 'interface Typesafety',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 27, character: 18},
                      end: {line: 27, character: 18},
                    },
                    newText: 'Typesafe',
                  },
                },
                {
                  label: 'Typhoon',
                  kind: 13,
                  detail: 'type Typhoon = string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 27, character: 18},
                      end: {line: 27, character: 18},
                    },
                    newText: 'Typhoon',
                  },
                },
                {
                  label: 'Typnotism',
                  kind: 13,
                  detail: 'type Typnotism = number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 27, character: 18},
                      end: {line: 27, character: 18},
                    },
                    newText: 'Typnotism',
                  },
                },
                {
                  label: 'Tyrant',
                  kind: 13,
                  detail: 'type Tyrant = string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 27, character: 18},
                      end: {line: 27, character: 18},
                    },
                    newText: 'Tyrant',
                  },
                },
                {
                  label: 'Tympanic',
                  kind: 13,
                  detail: 'type Tympanic = number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 27, character: 18},
                      end: {line: 27, character: 18},
                    },
                    newText: 'Tympanic',
                  },
                },
                {
                  label: 'Typeset',
                  kind: 8,
                  detail: 'interface Typeset',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 27, character: 18},
                      end: {line: 27, character: 18},
                    },
                    newText: 'Typeset',
                  },
                },
                {
                  label: 'Typaram',
                  kind: 25,
                  detail: 'Typaram',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 27, character: 18},
                      end: {line: 27, character: 18},
                    },
                    newText: 'Typaram',
                  },
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
): Suite);
