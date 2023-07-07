/*
 * @flow
 * @format
 */

import type {Suite} from 'flow-dev-tools/src/test/Suite';
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(
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
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'function',
                  kind: 14,
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'function',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'fAUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'function',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'for',
                  kind: 14,
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'for',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'fAUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'for',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'finally',
                  kind: 14,
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'finally',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'fAUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'finally',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'far',
                  kind: 6,
                  detail: '(global)',
                  sortText: '00000000000000000003',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'far',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'global',
                      {
                        token: 'fAUTO332',
                        index: 3,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'far',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'foo',
                  kind: 6,
                  detail: 'Import default from ./foo',
                  sortText: '00000000000000000004',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
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
                      {
                        token: 'fAUTO332',
                        index: 4,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'foo',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'foo',
                  kind: 6,
                  detail: 'Import from LibA',
                  sortText: '00000000000000000005',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText: 'import { foo } from "LibA";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'fAUTO332',
                        index: 5,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'foo',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'foo',
                  kind: 6,
                  detail: 'Import * from ./foo',
                  sortText: '00000000000000000006',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
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
                      {
                        token: 'fAUTO332',
                        index: 6,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'foo',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'foobar',
                  kind: 6,
                  detail: 'Import default from ./foobar',
                  sortText: '00000000000000000007',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'foobar',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
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
                      {
                        token: 'fAUTO332',
                        index: 7,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'foobar',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'Foo',
                  kind: 6,
                  detail: 'Import from ./foo',
                  sortText: '00000000000000000008',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'Foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText: 'import { Foo } from "./foo";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'fAUTO332',
                        index: 8,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'Foo',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'Function',
                  kind: 6,
                  detail: '(global)',
                  sortText: '00000000000000000009',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'Function',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'global',
                      {
                        token: 'fAUTO332',
                        index: 9,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'Function',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'barFoo',
                  kind: 6,
                  detail: 'Import from ./bar',
                  sortText: '00000000000000000010',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'barFoo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText: 'import { barFoo } from "./bar";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'fAUTO332',
                        index: 10,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'barFoo',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
              ],
            },
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    test('textDocument/completion with ranked autoimports', [
      addFiles('foo.js', 'bar.js', 'foobar.js', 'lib/builtins.js'),
      addCode(`f`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 1},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'function',
                  kind: 14,
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'function',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'fAUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'function',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'for',
                  kind: 14,
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'for',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'fAUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'for',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'finally',
                  kind: 14,
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'finally',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'fAUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'finally',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'Foo',
                  kind: 6,
                  detail: 'Import from ./foo',
                  sortText: '00000000000000000003',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'Foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText: 'import { Foo } from "./foo";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'fAUTO332',
                        index: 3,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'Foo',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'far',
                  kind: 6,
                  detail: '(global)',
                  sortText: '00000000000000000004',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'far',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'global',
                      {
                        token: 'fAUTO332',
                        index: 4,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'far',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'foo',
                  kind: 6,
                  detail: 'Import default from ./foo',
                  sortText: '00000000000000000005',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
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
                      {
                        token: 'fAUTO332',
                        index: 5,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'foo',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'foo',
                  kind: 6,
                  detail: 'Import from LibA',
                  sortText: '00000000000000000006',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText: 'import { foo } from "LibA";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'fAUTO332',
                        index: 6,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'foo',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'foo',
                  kind: 6,
                  detail: 'Import * from ./foo',
                  sortText: '00000000000000000007',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
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
                      {
                        token: 'fAUTO332',
                        index: 7,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'foo',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'foobar',
                  kind: 6,
                  detail: 'Import default from ./foobar',
                  sortText: '00000000000000000008',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'foobar',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
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
                      {
                        token: 'fAUTO332',
                        index: 8,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'foobar',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'barFoo',
                  kind: 6,
                  detail: 'Import from ./bar',
                  sortText: '00000000000000000009',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'barFoo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText: 'import { barFoo } from "./bar";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'fAUTO332',
                        index: 9,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'barFoo',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'Function',
                  kind: 6,
                  detail: '(global)',
                  sortText: '00000000000000000010',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 1,
                      },
                    },
                    newText: 'Function',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'global',
                      {
                        token: 'fAUTO332',
                        index: 10,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'Function',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
              ],
            },
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]).flowConfig('_flowconfig_ranked'),

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
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'Foo',
                  kind: 3,
                  detail: '(props: {...}) => null',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 4,
                        character: 2,
                      },
                      end: {
                        line: 4,
                        character: 3,
                      },
                    },
                    newText: 'Foo',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
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
                      {
                        token: 'FAUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'Foo',
                        ac_type: 'Ac_jsx_element',
                      },
                    ],
                  },
                },
                {
                  label: 'Function',
                  kind: 6,
                  detail: '(global)',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 4,
                        character: 2,
                      },
                      end: {
                        line: 4,
                        character: 3,
                      },
                    },
                    newText: 'Function',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
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
                      {
                        token: 'FAUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'Function',
                        ac_type: 'Ac_jsx_element',
                      },
                    ],
                  },
                },
              ],
            },
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    test('textDocument/completion on types', [
      addFiles('foo.js', 'types.js', 'lib/builtins.js'),
      addCode(`type Test = T`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 13},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'T',
                  kind: 6,
                  detail: 'Import type from ./types',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: 'T',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText: 'import type { T } from "./types";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'TAUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'T',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Test',
                  kind: 6,
                  detail: 'type Test = any',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: 'Test',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        token: 'TAUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'Test',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Toodle',
                  kind: 6,
                  detail: 'Import type from ./types',
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: 'Toodle',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText: 'import type { Toodle } from "./types";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'TAUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'Toodle',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'TaggedTemplateLiteralArray',
                  kind: 6,
                  detail: '(global)',
                  sortText: '00000000000000000003',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: 'TaggedTemplateLiteralArray',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'global',
                      {
                        token: 'TAUTO332',
                        index: 3,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'TaggedTemplateLiteralArray',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'true',
                  kind: 6,
                  detail: 'true',
                  sortText: '00000000000000000004',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: 'true',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'TAUTO332',
                        index: 4,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'true',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$TupleMap',
                  kind: 3,
                  detail: '$TupleMap',
                  sortText: '00000000000000000005',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: '$TupleMap',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'TAUTO332',
                        index: 5,
                        session_requests: 1,
                        typed_length: 1,
                        completion: '$TupleMap',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$ElementType',
                  kind: 3,
                  detail: '$ElementType',
                  sortText: '00000000000000000006',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: '$ElementType',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'TAUTO332',
                        index: 6,
                        session_requests: 1,
                        typed_length: 1,
                        completion: '$ElementType',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$NonMaybeType',
                  kind: 3,
                  detail: '$NonMaybeType',
                  sortText: '00000000000000000007',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: '$NonMaybeType',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'TAUTO332',
                        index: 7,
                        session_requests: 1,
                        typed_length: 1,
                        completion: '$NonMaybeType',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$PropertyType',
                  kind: 3,
                  detail: '$PropertyType',
                  sortText: '00000000000000000008',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: '$PropertyType',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'TAUTO332',
                        index: 8,
                        session_requests: 1,
                        typed_length: 1,
                        completion: '$PropertyType',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
              ],
            },
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    test('textDocument/completion on types with ranked autoimports', [
      addFiles('foo.js', 'types.js', 'lib/builtins.js'),
      addCode(`type Test = T`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 13},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'Test',
                  kind: 6,
                  detail: 'type Test = any',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: 'Test',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        token: 'TAUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'Test',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'true',
                  kind: 6,
                  detail: 'true',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: 'true',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'TAUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'true',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$TupleMap',
                  kind: 3,
                  detail: '$TupleMap',
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: '$TupleMap',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'TAUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 1,
                        completion: '$TupleMap',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$ElementType',
                  kind: 3,
                  detail: '$ElementType',
                  sortText: '00000000000000000003',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: '$ElementType',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'TAUTO332',
                        index: 3,
                        session_requests: 1,
                        typed_length: 1,
                        completion: '$ElementType',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$NonMaybeType',
                  kind: 3,
                  detail: '$NonMaybeType',
                  sortText: '00000000000000000004',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: '$NonMaybeType',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'TAUTO332',
                        index: 4,
                        session_requests: 1,
                        typed_length: 1,
                        completion: '$NonMaybeType',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$PropertyType',
                  kind: 3,
                  detail: '$PropertyType',
                  sortText: '00000000000000000005',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: '$PropertyType',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'TAUTO332',
                        index: 5,
                        session_requests: 1,
                        typed_length: 1,
                        completion: '$PropertyType',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'T',
                  kind: 6,
                  detail: 'Import type from ./types',
                  sortText: '00000000000000000006',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: 'T',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText: 'import type { T } from "./types";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'TAUTO332',
                        index: 6,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'T',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Toodle',
                  kind: 6,
                  detail: 'Import type from ./types',
                  sortText: '00000000000000000007',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: 'Toodle',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText: 'import type { Toodle } from "./types";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'TAUTO332',
                        index: 7,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'Toodle',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'TaggedTemplateLiteralArray',
                  kind: 6,
                  detail: '(global)',
                  sortText: '00000000000000000008',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 12,
                      },
                      end: {
                        line: 2,
                        character: 13,
                      },
                    },
                    newText: 'TaggedTemplateLiteralArray',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'global',
                      {
                        token: 'TAUTO332',
                        index: 8,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'TaggedTemplateLiteralArray',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
              ],
            },
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]).flowConfig('_flowconfig_ranked'),

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
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'not_null',
                  kind: 6,
                  detail: 'Import from ./reserved',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    newText: 'not_null',
                    insert: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 3,
                      },
                    },
                    replace: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 4,
                      },
                    },
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText: 'import { not_null } from "./reserved";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'nulAUTO332l',
                        index: 0,
                        session_requests: 1,
                        typed_length: 3,
                        completion: 'not_null',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
              ],
            },
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    // should not suggest importing `foobar` from foobar.js
    test('textDocument/completion should exclude variables already in scope', [
      addFiles('foobar.js'),
      addCode(`const foobar = ''; foobar`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 25},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'foobar',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 19,
                      },
                      end: {
                        line: 2,
                        character: 25,
                      },
                    },
                    newText: 'foobar',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'foobarAUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 6,
                        completion: 'foobar',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
              ],
            },
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    test('should sort properly', [
      addFiles('AllTheThings.js'),
      addCode(`All`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 3},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'AllTheThings',
                  kind: 6,
                  detail: 'Import default from ./AllTheThings',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 3,
                      },
                    },
                    newText: 'AllTheThings',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText: 'import AllTheThings from "./AllTheThings";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'AllAUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 3,
                        completion: 'AllTheThings',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'AllTheThings',
                  kind: 6,
                  detail: 'Import from ./AllTheThings',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 3,
                      },
                    },
                    newText: 'AllTheThings',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText:
                        'import { AllTheThings } from "./AllTheThings";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'AllAUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 3,
                        completion: 'AllTheThings',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'AllTheThings',
                  kind: 6,
                  detail: 'Import * from ./AllTheThings',
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 3,
                      },
                    },
                    newText: 'AllTheThings',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText:
                        'import * as AllTheThings from "./AllTheThings";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'AllAUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 3,
                        completion: 'AllTheThings',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
              ],
            },
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    test('should handle scoped module names', [
      addFiles('lib/scoped.js'),
      addCode(`xy`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 2},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'xyz',
                  kind: 6,
                  detail: 'Import default from @example/xyz',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 2,
                      },
                    },
                    newText: 'xyz',
                  },
                  additionalTextEdits: [
                    {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 2,
                          character: 0,
                        },
                      },
                      newText: 'import xyz from "@example/xyz";\n\n',
                    },
                  ],
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'autoimport',
                      {
                        token: 'xyAUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 2,
                        completion: 'xyz',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
              ],
            },
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
  ],
): Suite);
