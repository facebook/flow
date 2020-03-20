/*
 * @flow
 * @format
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
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
                  inlineDetail: 'this',
                  insertTextFormat: 1,
                },
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
                  inlineDetail: '(a: number, b: string) => number',
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
                  inlineDetail: 'this',
                  insertTextFormat: 1,
                },
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
                  inlineDetail: '() => void',
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
                  inlineDetail: '() => null',
                  insertTextFormat: 1,
                },
                {
                  label: 'aClass',
                  kind: 7,
                  detail: 'class aClass',
                  inlineDetail: 'class aClass',
                  insertTextFormat: 1,
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
                  inlineDetail: 'this',
                  insertTextFormat: 1,
                },
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
                  inlineDetail: '() => void',
                  insertTextFormat: 1,
                },
                {
                  label: 'aFunction',
                  kind: 3,
                  detail: '(arg1: number, arg2: string) => null',
                  inlineDetail: '(arg1: number, arg2: string) => null',
                  insertTextFormat: 1,
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
                  inlineDetail: 'number',
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
                  inlineDetail: 'module React',
                  insertTextFormat: 1,
                },
                {
                  label: 'D',
                  kind: 3,
                  detail: '(props: Props) => void',
                  inlineDetail: '(props: Props) => void',
                  insertTextFormat: 1,
                },
                {
                  label: 'C',
                  kind: 7,
                  detail: 'class C',
                  inlineDetail: 'class C',
                  insertTextFormat: 1,
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
                  inlineDetail: 'number',
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
                    inlineDetail: 'number',
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
                  inlineDetail: 'number',
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
                  inlineDetail: '(thisArg: any, argArray?: any) => any',
                  insertTextFormat: 1,
                },
                {
                  label: 'arguments',
                  kind: 6,
                  detail: 'any',
                  inlineDetail: 'any',
                  insertTextFormat: 1,
                },
                {
                  label: 'bind',
                  kind: 3,
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  inlineDetail:
                    '(thisArg: any, ...argArray: Array<any>) => any',
                  insertTextFormat: 1,
                },
                {
                  label: 'call',
                  kind: 3,
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  inlineDetail:
                    '(thisArg: any, ...argArray: Array<any>) => any',
                  insertTextFormat: 1,
                },
                {
                  label: 'caller',
                  kind: 13,
                  detail: 'any | null',
                  inlineDetail: 'any | null',
                  insertTextFormat: 1,
                },
                {
                  label: 'childContextTypes',
                  kind: 6,
                  detail: 'any',
                  inlineDetail: 'any',
                  insertTextFormat: 1,
                },
                {
                  label: 'contextTypes',
                  kind: 6,
                  detail: 'any',
                  inlineDetail: 'any',
                  insertTextFormat: 1,
                },
                {
                  label: 'displayName',
                  kind: 13,
                  detail: '(?string) | void',
                  inlineDetail: '(?string) | void',
                  insertTextFormat: 1,
                },
                {
                  label: 'hasOwnProperty',
                  kind: 3,
                  detail: '(prop: mixed) => boolean',
                  inlineDetail: '(prop: mixed) => boolean',
                  insertTextFormat: 1,
                },
                {
                  label: 'isPrototypeOf',
                  kind: 3,
                  detail: '(o: mixed) => boolean',
                  inlineDetail: '(o: mixed) => boolean',
                  insertTextFormat: 1,
                },
                {
                  label: 'length',
                  kind: 6,
                  detail: 'number',
                  inlineDetail: 'number',
                  insertTextFormat: 1,
                },
                {
                  label: 'name',
                  kind: 6,
                  detail: 'string',
                  inlineDetail: 'string',
                  insertTextFormat: 1,
                },
                {
                  label: 'propTypes',
                  kind: 6,
                  detail: 'any',
                  inlineDetail: 'any',
                  insertTextFormat: 1,
                },
                {
                  label: 'propertyIsEnumerable',
                  kind: 3,
                  detail: '(prop: mixed) => boolean',
                  inlineDetail: '(prop: mixed) => boolean',
                  insertTextFormat: 1,
                },
                {
                  label: 'toLocaleString',
                  kind: 3,
                  detail: '() => string',
                  inlineDetail: '() => string',
                  insertTextFormat: 1,
                },
                {
                  label: 'toString',
                  kind: 3,
                  detail: '() => string',
                  inlineDetail: '() => string',
                  insertTextFormat: 1,
                },
                {
                  label: 'valueOf',
                  kind: 3,
                  detail: '() => mixed',
                  inlineDetail: '() => mixed',
                  insertTextFormat: 1,
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
                  inlineDetail: '(thisArg: any, argArray?: any) => any',
                  insertTextFormat: 1,
                },
                {
                  label: 'arguments',
                  kind: 6,
                  detail: 'any',
                  inlineDetail: 'any',
                  insertTextFormat: 1,
                },
                {
                  label: 'bind',
                  kind: 3,
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  inlineDetail:
                    '(thisArg: any, ...argArray: Array<any>) => any',
                  insertTextFormat: 1,
                },
                {
                  label: 'call',
                  kind: 3,
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  inlineDetail:
                    '(thisArg: any, ...argArray: Array<any>) => any',
                  insertTextFormat: 1,
                },
                {
                  label: 'caller',
                  kind: 13,
                  detail: 'any | null',
                  inlineDetail: 'any | null',
                  insertTextFormat: 1,
                },
                {
                  label: 'childContextTypes',
                  kind: 6,
                  detail: 'any',
                  inlineDetail: 'any',
                  insertTextFormat: 1,
                },
                {
                  label: 'contextTypes',
                  kind: 6,
                  detail: 'any',
                  inlineDetail: 'any',
                  insertTextFormat: 1,
                },
                {
                  label: 'displayName',
                  kind: 13,
                  detail: '(?string) | void',
                  inlineDetail: '(?string) | void',
                  insertTextFormat: 1,
                },
                {
                  label: 'hasOwnProperty',
                  kind: 3,
                  detail: '(prop: mixed) => boolean',
                  inlineDetail: '(prop: mixed) => boolean',
                  insertTextFormat: 1,
                },
                {
                  label: 'isPrototypeOf',
                  kind: 3,
                  detail: '(o: mixed) => boolean',
                  inlineDetail: '(o: mixed) => boolean',
                  insertTextFormat: 1,
                },
                {
                  label: 'length',
                  kind: 6,
                  detail: 'number',
                  inlineDetail: 'number',
                  insertTextFormat: 1,
                },
                {
                  label: 'name',
                  kind: 6,
                  detail: 'string',
                  inlineDetail: 'string',
                  insertTextFormat: 1,
                },
                {
                  label: 'propTypes',
                  kind: 6,
                  detail: 'any',
                  inlineDetail: 'any',
                  insertTextFormat: 1,
                },
                {
                  label: 'propertyIsEnumerable',
                  kind: 3,
                  detail: '(prop: mixed) => boolean',
                  inlineDetail: '(prop: mixed) => boolean',
                  insertTextFormat: 1,
                },
                {
                  label: 'toLocaleString',
                  kind: 3,
                  detail: '() => string',
                  inlineDetail: '() => string',
                  insertTextFormat: 1,
                },
                {
                  label: 'toString',
                  kind: 3,
                  detail: '() => string',
                  inlineDetail: '() => string',
                  insertTextFormat: 1,
                },
                {
                  label: 'valueOf',
                  kind: 3,
                  detail: '() => mixed',
                  inlineDetail: '() => mixed',
                  insertTextFormat: 1,
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
                  inlineDetail: 'module Typologies',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                },
                {
                  label: 'Typography',
                  kind: 7,
                  detail: 'class Typewriter',
                  inlineDetail: 'class Typewriter',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                },
                {
                  label: 'Typewriter',
                  kind: 7,
                  detail: 'class Typewriter',
                  inlineDetail: 'class Typewriter',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                },
                {
                  label: 'Types',
                  kind: 9,
                  detail: 'module Types',
                  inlineDetail: 'module Types',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                },
                {
                  label: 'Typesafe',
                  kind: 8,
                  detail: 'interface Typesafety',
                  inlineDetail: 'interface Typesafety',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                },
                {
                  label: 'Typhoon',
                  kind: 13,
                  detail: 'type Typhoon = string',
                  inlineDetail: 'type Typhoon = string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                },
                {
                  label: 'Typnotism',
                  kind: 13,
                  detail: 'type Typnotism = number',
                  inlineDetail: 'type Typnotism = number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                },
                {
                  label: 'Tyrant',
                  kind: 13,
                  detail: 'type Tyrant = string',
                  inlineDetail: 'type Tyrant = string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                },
                {
                  label: 'Tympanic',
                  kind: 13,
                  detail: 'type Tympanic = number',
                  inlineDetail: 'type Tympanic = number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                },
                {
                  label: 'Typeset',
                  kind: 8,
                  detail: 'interface Typeset',
                  inlineDetail: 'interface Typeset',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                },
                {
                  label: 'Typaram',
                  kind: 25,
                  detail: 'Typaram',
                  inlineDetail: 'Typaram',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
);
