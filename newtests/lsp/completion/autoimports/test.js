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
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'global',
                      {token: 'fAUTO332', completion: 'undefined'},
                    ],
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
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'global',
                      {token: 'fAUTO332', completion: 'Function'},
                    ],
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
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {token: 'fAUTO332', completion: 'foobar'},
                    ],
                  },
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
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {token: 'fAUTO332', completion: 'foo'},
                    ],
                  },
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
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {token: 'fAUTO332', completion: 'foo'},
                    ],
                  },
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
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {token: 'fAUTO332', completion: 'foo'},
                    ],
                  },
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
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {token: 'fAUTO332', completion: 'Foo'},
                    ],
                  },
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]).flowConfig('_flowconfig_autoimports'),

    test('textDocument/completion with JSX autoimports', [
      addCode(`function Foo(props: {...}): null {}`),
      addCode(`(<F`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 4, character: 3},
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
                      start: {line: 4, character: 2},
                      end: {line: 4, character: 3},
                    },
                    newText: 'undefined',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {line: 2, character: 0},
                        end: {line: 2, character: 0},
                      },
                      newText: 'import * as React from "react";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'global',
                      {token: 'FAUTO332', completion: 'undefined'},
                    ],
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
                      start: {line: 4, character: 2},
                      end: {line: 4, character: 3},
                    },
                    newText: 'Function',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {line: 2, character: 0},
                        end: {line: 2, character: 0},
                      },
                      newText: 'import * as React from "react";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'global',
                      {token: 'FAUTO332', completion: 'Function'},
                    ],
                  },
                },
                {
                  label: 'Foo',
                  kind: 3,
                  detail: '(props: {...}) => null',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 4, character: 2},
                      end: {line: 4, character: 3},
                    },
                    newText: 'Foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {line: 2, character: 0},
                        end: {line: 2, character: 0},
                      },
                      newText: 'import * as React from "react";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {token: 'FAUTO332', completion: 'Foo'},
                    ],
                  },
                },
              ],
            }),
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]).flowConfig('_flowconfig_autoimports'),

    test('textDocument/completion should exclude reserved words', [
      addFiles('reserved.js'),
      addCode(`null`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 3},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/completion',
            JSON.stringify({
              isIncomplete: false,
              items: [
                {
                  label: 'not_null',
                  kind: 6,
                  detail: 'not_null',
                  documentation: {
                    kind: 'markdown',
                    value: 'Import from ./reserved',
                  },
                  sortText: '00000000000000000100',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {line: 2, character: 0},
                      end: {line: 2, character: 4},
                    },
                    newText: 'not_null',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {line: 2, character: 0},
                        end: {line: 2, character: 0},
                      },
                      newText: 'import {not_null} from "./reserved";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {token: 'nulAUTO332l', completion: 'not_null'},
                    ],
                  },
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
