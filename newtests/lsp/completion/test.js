/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {suite, test} = require('../../Tester');

module.exports = (suite(
  ({
    lspStartAndConnect,
    lspStart,
    lspRequest,
    lspInitializeParams,
    lspRequestAndWaitUntilResponse,
    addFile,
    lspNotification,
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
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'a',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'a',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'a',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'async',
                  kind: 14,
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'async',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'async',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'await',
                  kind: 14,
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'await',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'await',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'b',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000003',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'b',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 3,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'b',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'break',
                  kind: 14,
                  sortText: '00000000000000000004',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'break',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 4,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'break',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'case',
                  kind: 14,
                  sortText: '00000000000000000005',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'case',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 5,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'case',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'catch',
                  kind: 14,
                  sortText: '00000000000000000006',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'catch',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 6,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'catch',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'class',
                  kind: 14,
                  sortText: '00000000000000000007',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'class',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 7,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'class',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'const',
                  kind: 14,
                  sortText: '00000000000000000008',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'const',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 8,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'const',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'continue',
                  kind: 14,
                  sortText: '00000000000000000009',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'continue',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 9,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'continue',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'debugger',
                  kind: 14,
                  sortText: '00000000000000000010',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'debugger',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 10,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'debugger',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'declare',
                  kind: 14,
                  sortText: '00000000000000000011',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'declare',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 11,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'declare',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'default',
                  kind: 14,
                  sortText: '00000000000000000012',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'default',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 12,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'default',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'delete',
                  kind: 14,
                  sortText: '00000000000000000013',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'delete',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 13,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'delete',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'do',
                  kind: 14,
                  sortText: '00000000000000000014',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'do',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 14,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'do',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'else',
                  kind: 14,
                  sortText: '00000000000000000015',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'else',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 15,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'else',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'enum',
                  kind: 14,
                  sortText: '00000000000000000016',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'enum',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 16,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'enum',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'export',
                  kind: 14,
                  sortText: '00000000000000000017',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'export',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 17,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'export',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'extends',
                  kind: 14,
                  sortText: '00000000000000000018',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'extends',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 18,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'extends',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'finally',
                  kind: 14,
                  sortText: '00000000000000000019',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
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
                        token: 'AUTO332',
                        index: 19,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'finally',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'for',
                  kind: 14,
                  sortText: '00000000000000000020',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
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
                        token: 'AUTO332',
                        index: 20,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'for',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'fred',
                  kind: 3,
                  detail: '(a: number, b: string) => number',
                  documentation: {
                    kind: 'markdown',
                    value:
                      "Docblock for 'fred'\n\n**@return** {number} Docblock for return",
                  },
                  sortText: '00000000000000000021',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'fred',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 21,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'fred',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'function',
                  kind: 14,
                  sortText: '00000000000000000022',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
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
                        token: 'AUTO332',
                        index: 22,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'function',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'if',
                  kind: 14,
                  sortText: '00000000000000000023',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'if',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 23,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'if',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'implements',
                  kind: 14,
                  sortText: '00000000000000000024',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'implements',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 24,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'implements',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'import',
                  kind: 14,
                  sortText: '00000000000000000025',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'import',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 25,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'import',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'interface',
                  kind: 14,
                  sortText: '00000000000000000026',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'interface',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 26,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'interface',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'let',
                  kind: 14,
                  sortText: '00000000000000000027',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'let',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 27,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'let',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'new',
                  kind: 14,
                  sortText: '00000000000000000028',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'new',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 28,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'new',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'opaque',
                  kind: 14,
                  sortText: '00000000000000000029',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'opaque',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 29,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'opaque',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'return',
                  kind: 14,
                  sortText: '00000000000000000030',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'return',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 30,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'return',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'static',
                  kind: 14,
                  sortText: '00000000000000000031',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'static',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 31,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'static',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'switch',
                  kind: 14,
                  sortText: '00000000000000000032',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'switch',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 32,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'switch',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'this',
                  kind: 6,
                  detail: 'this',
                  sortText: '00000000000000000033',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'this',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'this',
                      {
                        token: 'AUTO332',
                        index: 33,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'this',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'throw',
                  kind: 14,
                  sortText: '00000000000000000034',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'throw',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 34,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'throw',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'try',
                  kind: 14,
                  sortText: '00000000000000000035',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'try',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 35,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'try',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'type',
                  kind: 14,
                  sortText: '00000000000000000036',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'type',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 36,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'type',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'typeof',
                  kind: 14,
                  sortText: '00000000000000000037',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'typeof',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 37,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'typeof',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'var',
                  kind: 14,
                  sortText: '00000000000000000038',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'var',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 38,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'var',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'void',
                  kind: 14,
                  sortText: '00000000000000000039',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'void',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 39,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'void',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'while',
                  kind: 14,
                  sortText: '00000000000000000040',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'while',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 40,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'while',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'x',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000041',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'x',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 41,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'x',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'yield',
                  kind: 14,
                  sortText: '00000000000000000042',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 10,
                        character: 15,
                      },
                      end: {
                        line: 10,
                        character: 15,
                      },
                    },
                    newText: 'yield',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 42,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'yield',
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
    test('textDocument/completion', [
      addFile('completion-metrics.js'),
      addFile('completion_metrics_collision.js'),
      lspStartAndConnect(),
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/completion-metrics.js',
          languageId: 'flow',
          version: 1,
          text: `// @flow
declare class A{
  test: string;
}

const b = new A();
b.
`,
        },
      }).verifyAllLSPMessagesInStep(
        [],
        ['window/showStatus', 'textDocument/publishDiagnostics'],
      ),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/completion-metrics.js'},
        position: {line: 6, character: 2},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'test',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 6,
                        character: 2,
                      },
                      end: {
                        line: 6,
                        character: 2,
                      },
                    },
                    newText: 'test',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'test',
                        ac_type: 'Acmem',
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
      lspNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/completion-metrics.js',
          version: 2,
        },
        contentChanges: [
          {
            range: {
              start: {
                line: 6,
                character: 2,
              },
              end: {
                line: 6,
                character: 2,
              },
            },
            rangeLength: 0,
            text: 't',
          },
        ],
      }).verifyAllLSPMessagesInStep(
        [],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/completion-metrics.js'},
        position: {line: 6, character: 3},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'test',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 6,
                        character: 2,
                      },
                      end: {
                        line: 6,
                        character: 3,
                      },
                    },
                    newText: 'test',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'tAUTO332',
                        index: 0,
                        session_requests: 2,
                        typed_length: 1,
                        completion: 'test',
                        ac_type: 'Acmem',
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
      lspNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/completion-metrics.js',
          version: 3,
        },
        contentChanges: [
          {
            text: `// @flow
declare class A{
  test: string;
}

const b = new A();

b.te
`,
          },
        ],
      }).verifyAllLSPMessagesInStep(
        [],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/completion-metrics.js'},
        position: {line: 7, character: 2},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'test',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    newText: 'test',
                    insert: {
                      start: {
                        line: 7,
                        character: 2,
                      },
                      end: {
                        line: 7,
                        character: 2,
                      },
                    },
                    replace: {
                      start: {
                        line: 7,
                        character: 2,
                      },
                      end: {
                        line: 7,
                        character: 4,
                      },
                    },
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332te',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'test',
                        ac_type: 'Acmem',
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
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/completion-metrics.js'},
        position: {line: 7, character: 3},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'test',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    newText: 'test',
                    insert: {
                      start: {
                        line: 7,
                        character: 2,
                      },
                      end: {
                        line: 7,
                        character: 3,
                      },
                    },
                    replace: {
                      start: {
                        line: 7,
                        character: 2,
                      },
                      end: {
                        line: 7,
                        character: 4,
                      },
                    },
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'tAUTO332e',
                        index: 0,
                        session_requests: 2,
                        typed_length: 1,
                        completion: 'test',
                        ac_type: 'Acmem',
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
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/completion-metrics.js'},
        position: {line: 7, character: 4},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'test',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 7,
                        character: 2,
                      },
                      end: {
                        line: 7,
                        character: 4,
                      },
                    },
                    newText: 'test',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'teAUTO332',
                        index: 0,
                        session_requests: 3,
                        typed_length: 2,
                        completion: 'test',
                        ac_type: 'Acmem',
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
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/completion_metrics_collision.js',
        },
        position: {line: 7, character: 4},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'test',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 7,
                        character: 2,
                      },
                      end: {
                        line: 7,
                        character: 4,
                      },
                    },
                    newText: 'test',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'teAUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 2,
                        completion: 'test',
                        ac_type: 'Acmem',
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
    test('textDocument/completion', [
      addFile('kind.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/kind.js'},
        position: {line: 13, character: 15},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'aClass',
                  kind: 7,
                  detail: 'class aClass',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'aClass',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'aClass',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'aFunction',
                  kind: 3,
                  detail: '() => null',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'aFunction',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'aFunction',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'aNumber',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'aNumber',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'aNumber',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'async',
                  kind: 14,
                  sortText: '00000000000000000003',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'async',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 3,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'async',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'await',
                  kind: 14,
                  sortText: '00000000000000000004',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'await',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 4,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'await',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'break',
                  kind: 14,
                  sortText: '00000000000000000005',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'break',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 5,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'break',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'case',
                  kind: 14,
                  sortText: '00000000000000000006',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'case',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 6,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'case',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'catch',
                  kind: 14,
                  sortText: '00000000000000000007',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'catch',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 7,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'catch',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'class',
                  kind: 14,
                  sortText: '00000000000000000008',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'class',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 8,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'class',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'const',
                  kind: 14,
                  sortText: '00000000000000000009',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'const',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 9,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'const',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'continue',
                  kind: 14,
                  sortText: '00000000000000000010',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'continue',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 10,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'continue',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'debugger',
                  kind: 14,
                  sortText: '00000000000000000011',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'debugger',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 11,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'debugger',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'declare',
                  kind: 14,
                  sortText: '00000000000000000012',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'declare',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 12,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'declare',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'default',
                  kind: 14,
                  sortText: '00000000000000000013',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'default',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 13,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'default',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'delete',
                  kind: 14,
                  sortText: '00000000000000000014',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'delete',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 14,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'delete',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'do',
                  kind: 14,
                  sortText: '00000000000000000015',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'do',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 15,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'do',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'else',
                  kind: 14,
                  sortText: '00000000000000000016',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'else',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 16,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'else',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'enum',
                  kind: 14,
                  sortText: '00000000000000000017',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'enum',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 17,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'enum',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'export',
                  kind: 14,
                  sortText: '00000000000000000018',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'export',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 18,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'export',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'extends',
                  kind: 14,
                  sortText: '00000000000000000019',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'extends',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 19,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'extends',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'finally',
                  kind: 14,
                  sortText: '00000000000000000020',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
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
                        token: 'AUTO332',
                        index: 20,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'finally',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'foo',
                  kind: 3,
                  detail: '() => void',
                  sortText: '00000000000000000021',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'foo',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 21,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'foo',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'for',
                  kind: 14,
                  sortText: '00000000000000000022',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
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
                        token: 'AUTO332',
                        index: 22,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'for',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'function',
                  kind: 14,
                  sortText: '00000000000000000023',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
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
                        token: 'AUTO332',
                        index: 23,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'function',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'if',
                  kind: 14,
                  sortText: '00000000000000000024',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'if',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 24,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'if',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'implements',
                  kind: 14,
                  sortText: '00000000000000000025',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'implements',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 25,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'implements',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'import',
                  kind: 14,
                  sortText: '00000000000000000026',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'import',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 26,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'import',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'interface',
                  kind: 14,
                  sortText: '00000000000000000027',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'interface',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 27,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'interface',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'let',
                  kind: 14,
                  sortText: '00000000000000000028',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'let',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 28,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'let',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'new',
                  kind: 14,
                  sortText: '00000000000000000029',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'new',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 29,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'new',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'opaque',
                  kind: 14,
                  sortText: '00000000000000000030',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'opaque',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 30,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'opaque',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'return',
                  kind: 14,
                  sortText: '00000000000000000031',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'return',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 31,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'return',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'static',
                  kind: 14,
                  sortText: '00000000000000000032',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'static',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 32,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'static',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'switch',
                  kind: 14,
                  sortText: '00000000000000000033',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'switch',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 33,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'switch',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'this',
                  kind: 6,
                  detail: 'this',
                  sortText: '00000000000000000034',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'this',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'this',
                      {
                        token: 'AUTO332',
                        index: 34,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'this',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'throw',
                  kind: 14,
                  sortText: '00000000000000000035',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'throw',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 35,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'throw',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'try',
                  kind: 14,
                  sortText: '00000000000000000036',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'try',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 36,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'try',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'type',
                  kind: 14,
                  sortText: '00000000000000000037',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'type',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 37,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'type',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'typeof',
                  kind: 14,
                  sortText: '00000000000000000038',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'typeof',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 38,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'typeof',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'var',
                  kind: 14,
                  sortText: '00000000000000000039',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'var',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 39,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'var',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'void',
                  kind: 14,
                  sortText: '00000000000000000040',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'void',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 40,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'void',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'while',
                  kind: 14,
                  sortText: '00000000000000000041',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'while',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 41,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'while',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'x',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000042',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'x',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 42,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'x',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'yield',
                  kind: 14,
                  sortText: '00000000000000000043',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 15,
                      },
                      end: {
                        line: 13,
                        character: 15,
                      },
                    },
                    newText: 'yield',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 43,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'yield',
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
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'aFunction',
                  kind: 3,
                  detail: '(arg1: number, arg2: string) => null',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'aFunction',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'aFunction',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'async',
                  kind: 14,
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'async',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'async',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'await',
                  kind: 14,
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'await',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'await',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'break',
                  kind: 14,
                  sortText: '00000000000000000003',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'break',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 3,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'break',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'case',
                  kind: 14,
                  sortText: '00000000000000000004',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'case',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 4,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'case',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'catch',
                  kind: 14,
                  sortText: '00000000000000000005',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'catch',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 5,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'catch',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'class',
                  kind: 14,
                  sortText: '00000000000000000006',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'class',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 6,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'class',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'const',
                  kind: 14,
                  sortText: '00000000000000000007',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'const',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 7,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'const',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'continue',
                  kind: 14,
                  sortText: '00000000000000000008',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'continue',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 8,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'continue',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'debugger',
                  kind: 14,
                  sortText: '00000000000000000009',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'debugger',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 9,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'debugger',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'declare',
                  kind: 14,
                  sortText: '00000000000000000010',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'declare',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 10,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'declare',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'default',
                  kind: 14,
                  sortText: '00000000000000000011',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'default',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 11,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'default',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'delete',
                  kind: 14,
                  sortText: '00000000000000000012',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'delete',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 12,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'delete',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'do',
                  kind: 14,
                  sortText: '00000000000000000013',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'do',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 13,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'do',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'else',
                  kind: 14,
                  sortText: '00000000000000000014',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'else',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 14,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'else',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'enum',
                  kind: 14,
                  sortText: '00000000000000000015',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'enum',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 15,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'enum',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'export',
                  kind: 14,
                  sortText: '00000000000000000016',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'export',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 16,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'export',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'extends',
                  kind: 14,
                  sortText: '00000000000000000017',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'extends',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 17,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'extends',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'finally',
                  kind: 14,
                  sortText: '00000000000000000018',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
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
                        token: 'AUTO332',
                        index: 18,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'finally',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'foo',
                  kind: 3,
                  detail: '() => void',
                  sortText: '00000000000000000019',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'foo',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 19,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'foo',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'for',
                  kind: 14,
                  sortText: '00000000000000000020',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
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
                        token: 'AUTO332',
                        index: 20,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'for',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'function',
                  kind: 14,
                  sortText: '00000000000000000021',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
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
                        token: 'AUTO332',
                        index: 21,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'function',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'if',
                  kind: 14,
                  sortText: '00000000000000000022',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'if',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 22,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'if',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'implements',
                  kind: 14,
                  sortText: '00000000000000000023',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'implements',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 23,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'implements',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'import',
                  kind: 14,
                  sortText: '00000000000000000024',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'import',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 24,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'import',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'interface',
                  kind: 14,
                  sortText: '00000000000000000025',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'interface',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 25,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'interface',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'let',
                  kind: 14,
                  sortText: '00000000000000000026',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'let',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 26,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'let',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'new',
                  kind: 14,
                  sortText: '00000000000000000027',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'new',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 27,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'new',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'opaque',
                  kind: 14,
                  sortText: '00000000000000000028',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'opaque',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 28,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'opaque',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'return',
                  kind: 14,
                  sortText: '00000000000000000029',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'return',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 29,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'return',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'static',
                  kind: 14,
                  sortText: '00000000000000000030',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'static',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 30,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'static',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'switch',
                  kind: 14,
                  sortText: '00000000000000000031',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'switch',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 31,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'switch',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'this',
                  kind: 6,
                  detail: 'this',
                  sortText: '00000000000000000032',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'this',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'this',
                      {
                        token: 'AUTO332',
                        index: 32,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'this',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'throw',
                  kind: 14,
                  sortText: '00000000000000000033',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'throw',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 33,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'throw',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'try',
                  kind: 14,
                  sortText: '00000000000000000034',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'try',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 34,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'try',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'type',
                  kind: 14,
                  sortText: '00000000000000000035',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'type',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 35,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'type',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'typeof',
                  kind: 14,
                  sortText: '00000000000000000036',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'typeof',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 36,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'typeof',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'var',
                  kind: 14,
                  sortText: '00000000000000000037',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'var',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 37,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'var',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'void',
                  kind: 14,
                  sortText: '00000000000000000038',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'void',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 38,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'void',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'while',
                  kind: 14,
                  sortText: '00000000000000000039',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'while',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 39,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'while',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'x',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000040',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'x',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 40,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'x',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'yield',
                  kind: 14,
                  sortText: '00000000000000000041',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 15,
                      },
                      end: {
                        line: 9,
                        character: 15,
                      },
                    },
                    newText: 'yield',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 41,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'yield',
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
    test('textDocument/completion triggered by space in jsx', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 12, character: 4},
        context: {triggerKind: 2, triggerCharacter: ' '},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'a',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 12,
                        character: 4,
                      },
                      end: {
                        line: 12,
                        character: 4,
                      },
                    },
                    newText: 'a=',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'jsx attribute',
                      {
                        token: 'AUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'a',
                        ac_type: 'Acjsx',
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
    test('textDocument/completion triggered by space outside of jsx', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 11, character: 1},
        context: {triggerKind: 2, triggerCharacter: ' '},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [],
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
    test('textDocument/completion invoked outside of jsx', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 11, character: 1},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'async',
                  kind: 14,
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 11,
                        character: 1,
                      },
                      end: {
                        line: 11,
                        character: 1,
                      },
                    },
                    newText: 'async',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'async',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'await',
                  kind: 14,
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 11,
                        character: 1,
                      },
                      end: {
                        line: 11,
                        character: 1,
                      },
                    },
                    newText: 'await',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'await',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'C',
                  kind: 7,
                  detail: 'class C',
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 11,
                        character: 1,
                      },
                      end: {
                        line: 11,
                        character: 1,
                      },
                    },
                    newText: 'C',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'C',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'class',
                  kind: 14,
                  sortText: '00000000000000000003',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 11,
                        character: 1,
                      },
                      end: {
                        line: 11,
                        character: 1,
                      },
                    },
                    newText: 'class',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 3,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'class',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'D',
                  kind: 3,
                  detail: '(props: Props) => void',
                  sortText: '00000000000000000004',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 11,
                        character: 1,
                      },
                      end: {
                        line: 11,
                        character: 1,
                      },
                    },
                    newText: 'D',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 4,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'D',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'delete',
                  kind: 14,
                  sortText: '00000000000000000005',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 11,
                        character: 1,
                      },
                      end: {
                        line: 11,
                        character: 1,
                      },
                    },
                    newText: 'delete',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 5,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'delete',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'function',
                  kind: 14,
                  sortText: '00000000000000000006',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 11,
                        character: 1,
                      },
                      end: {
                        line: 11,
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
                        token: 'AUTO332',
                        index: 6,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'function',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'import',
                  kind: 14,
                  sortText: '00000000000000000007',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 11,
                        character: 1,
                      },
                      end: {
                        line: 11,
                        character: 1,
                      },
                    },
                    newText: 'import',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 7,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'import',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'new',
                  kind: 14,
                  sortText: '00000000000000000008',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 11,
                        character: 1,
                      },
                      end: {
                        line: 11,
                        character: 1,
                      },
                    },
                    newText: 'new',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 8,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'new',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'React',
                  kind: 9,
                  detail: 'module "react"',
                  sortText: '00000000000000000009',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 11,
                        character: 1,
                      },
                      end: {
                        line: 11,
                        character: 1,
                      },
                    },
                    newText: 'React',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 9,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'React',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'typeof',
                  kind: 14,
                  sortText: '00000000000000000010',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 11,
                        character: 1,
                      },
                      end: {
                        line: 11,
                        character: 1,
                      },
                    },
                    newText: 'typeof',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 10,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'typeof',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'void',
                  kind: 14,
                  sortText: '00000000000000000011',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 11,
                        character: 1,
                      },
                      end: {
                        line: 11,
                        character: 1,
                      },
                    },
                    newText: 'void',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 11,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'void',
                        ac_type: 'Acid',
                      },
                    ],
                  },
                },
                {
                  label: 'yield',
                  kind: 14,
                  sortText: '00000000000000000012',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 11,
                        character: 1,
                      },
                      end: {
                        line: 11,
                        character: 1,
                      },
                    },
                    newText: 'yield',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        token: 'AUTO332',
                        index: 12,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'yield',
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
    test('textDocument/completion invoked in jsx', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 12, character: 4},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'a',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 12,
                        character: 4,
                      },
                      end: {
                        line: 12,
                        character: 4,
                      },
                    },
                    newText: 'a=',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'jsx attribute',
                      {
                        token: 'AUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'a',
                        ac_type: 'Acjsx',
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
            {
              method: 'textDocument/completion',
              result: {
                isIncomplete: false,
                items: [
                  {
                    label: 'a',
                    kind: 6,
                    detail: 'number',
                    sortText: '00000000000000000000',
                    insertTextFormat: 1,
                    textEdit: {
                      range: {
                        start: {
                          line: 13,
                          character: 4,
                        },
                        end: {
                          line: 13,
                          character: 4,
                        },
                      },
                      newText: 'a=',
                    },
                    command: {
                      title: '',
                      command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                      arguments: [
                        'textDocument/completion',
                        'jsx attribute',
                        {
                          token: 'AUTO332',
                          index: 0,
                          session_requests: 1,
                          typed_length: 0,
                          completion: 'a',
                          ac_type: 'Acjsx',
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
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'a',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 13,
                        character: 4,
                      },
                      end: {
                        line: 13,
                        character: 4,
                      },
                    },
                    newText: 'a=',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'jsx attribute',
                      {
                        token: 'AUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'a',
                        ac_type: 'Acjsx',
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
    test('textDocument/completion triggered by dot in jsx', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 14, character: 3},
        context: {triggerKind: 2, triggerCharacter: '.'},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'childContextTypes',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 3,
                      },
                      end: {
                        line: 14,
                        character: 3,
                      },
                    },
                    newText: 'childContextTypes',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'childContextTypes',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'contextTypes',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 3,
                      },
                      end: {
                        line: 14,
                        character: 3,
                      },
                    },
                    newText: 'contextTypes',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'contextTypes',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'displayName',
                  kind: 13,
                  detail: '(?string) | void',
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 3,
                      },
                      end: {
                        line: 14,
                        character: 3,
                      },
                    },
                    newText: 'displayName',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'displayName',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'propTypes',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000003',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 3,
                      },
                      end: {
                        line: 14,
                        character: 3,
                      },
                    },
                    newText: 'propTypes',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 3,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'propTypes',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'apply',
                  kind: 3,
                  detail: '(thisArg: any, argArray?: any) => any',
                  sortText: '00000000000000000004',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 3,
                      },
                      end: {
                        line: 14,
                        character: 3,
                      },
                    },
                    newText: 'apply',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 4,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'apply',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'arguments',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000005',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 3,
                      },
                      end: {
                        line: 14,
                        character: 3,
                      },
                    },
                    newText: 'arguments',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 5,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'arguments',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'bind',
                  kind: 3,
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  sortText: '00000000000000000006',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 3,
                      },
                      end: {
                        line: 14,
                        character: 3,
                      },
                    },
                    newText: 'bind',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 6,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'bind',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'call',
                  kind: 3,
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  sortText: '00000000000000000007',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 3,
                      },
                      end: {
                        line: 14,
                        character: 3,
                      },
                    },
                    newText: 'call',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 7,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'call',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'caller',
                  kind: 13,
                  detail: 'any | null',
                  sortText: '00000000000000000008',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 3,
                      },
                      end: {
                        line: 14,
                        character: 3,
                      },
                    },
                    newText: 'caller',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 8,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'caller',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'length',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000009',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 3,
                      },
                      end: {
                        line: 14,
                        character: 3,
                      },
                    },
                    newText: 'length',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 9,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'length',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'name',
                  kind: 6,
                  detail: 'string',
                  documentation: {
                    kind: 'markdown',
                    value: 'Returns the name of the function.',
                  },
                  sortText: '00000000000000000010',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 3,
                      },
                      end: {
                        line: 14,
                        character: 3,
                      },
                    },
                    newText: 'name',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 10,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'name',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'toString',
                  kind: 3,
                  detail: '() => string',
                  documentation: {
                    kind: 'markdown',
                    value: 'Returns a string representation of a function.',
                  },
                  sortText: '00000000000000000011',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 3,
                      },
                      end: {
                        line: 14,
                        character: 3,
                      },
                    },
                    newText: 'toString',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 11,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'toString',
                        ac_type: 'Acmem',
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
    test('textDocument/completion triggered by dot outside jsx', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 15, character: 2},
        context: {triggerKind: 2, triggerCharacter: '.'},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'childContextTypes',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 15,
                        character: 2,
                      },
                      end: {
                        line: 15,
                        character: 2,
                      },
                    },
                    newText: 'childContextTypes',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'childContextTypes',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'contextTypes',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 15,
                        character: 2,
                      },
                      end: {
                        line: 15,
                        character: 2,
                      },
                    },
                    newText: 'contextTypes',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'contextTypes',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'displayName',
                  kind: 13,
                  detail: '(?string) | void',
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 15,
                        character: 2,
                      },
                      end: {
                        line: 15,
                        character: 2,
                      },
                    },
                    newText: 'displayName',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'displayName',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'propTypes',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000003',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 15,
                        character: 2,
                      },
                      end: {
                        line: 15,
                        character: 2,
                      },
                    },
                    newText: 'propTypes',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 3,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'propTypes',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'apply',
                  kind: 3,
                  detail: '(thisArg: any, argArray?: any) => any',
                  sortText: '00000000000000000004',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 15,
                        character: 2,
                      },
                      end: {
                        line: 15,
                        character: 2,
                      },
                    },
                    newText: 'apply',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 4,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'apply',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'arguments',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000005',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 15,
                        character: 2,
                      },
                      end: {
                        line: 15,
                        character: 2,
                      },
                    },
                    newText: 'arguments',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 5,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'arguments',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'bind',
                  kind: 3,
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  sortText: '00000000000000000006',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 15,
                        character: 2,
                      },
                      end: {
                        line: 15,
                        character: 2,
                      },
                    },
                    newText: 'bind',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 6,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'bind',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'call',
                  kind: 3,
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  sortText: '00000000000000000007',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 15,
                        character: 2,
                      },
                      end: {
                        line: 15,
                        character: 2,
                      },
                    },
                    newText: 'call',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 7,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'call',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'caller',
                  kind: 13,
                  detail: 'any | null',
                  sortText: '00000000000000000008',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 15,
                        character: 2,
                      },
                      end: {
                        line: 15,
                        character: 2,
                      },
                    },
                    newText: 'caller',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 8,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'caller',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'length',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000009',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 15,
                        character: 2,
                      },
                      end: {
                        line: 15,
                        character: 2,
                      },
                    },
                    newText: 'length',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 9,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'length',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'name',
                  kind: 6,
                  detail: 'string',
                  documentation: {
                    kind: 'markdown',
                    value: 'Returns the name of the function.',
                  },
                  sortText: '00000000000000000010',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 15,
                        character: 2,
                      },
                      end: {
                        line: 15,
                        character: 2,
                      },
                    },
                    newText: 'name',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 10,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'name',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'toString',
                  kind: 3,
                  detail: '() => string',
                  documentation: {
                    kind: 'markdown',
                    value: 'Returns a string representation of a function.',
                  },
                  sortText: '00000000000000000011',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 15,
                        character: 2,
                      },
                      end: {
                        line: 15,
                        character: 2,
                      },
                    },
                    newText: 'toString',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        token: 'AUTO332',
                        index: 11,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'toString',
                        ac_type: 'Acmem',
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
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'Tympanic',
                  kind: 6,
                  detail: 'type Tympanic = number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Tympanic',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        token: 'AUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Tympanic',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Typaram',
                  kind: 25,
                  detail: 'Typaram',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Typaram',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type parameter',
                      {
                        token: 'AUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Typaram',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Types',
                  kind: 9,
                  detail: 'module "./type-exports.js"',
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Types.',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type -> qualified type',
                      {
                        token: 'AUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Types',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Typesafe',
                  kind: 8,
                  detail: 'interface Typesafe',
                  sortText: '00000000000000000003',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Typesafe',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        token: 'AUTO332',
                        index: 3,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Typesafe',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Typeset',
                  kind: 8,
                  detail: 'interface Typeset',
                  sortText: '00000000000000000004',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Typeset',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        token: 'AUTO332',
                        index: 4,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Typeset',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Typewriter',
                  kind: 7,
                  detail: 'class Typewriter',
                  sortText: '00000000000000000005',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Typewriter',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: class or enum',
                      {
                        token: 'AUTO332',
                        index: 5,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Typewriter',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Typhoon',
                  kind: 6,
                  detail: 'type Typhoon = string',
                  sortText: '00000000000000000006',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Typhoon',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        token: 'AUTO332',
                        index: 6,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Typhoon',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Typnotism',
                  kind: 6,
                  detail: 'type Typnotism = number',
                  sortText: '00000000000000000007',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Typnotism',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        token: 'AUTO332',
                        index: 7,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Typnotism',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Typography',
                  kind: 7,
                  detail: 'class Typewriter',
                  sortText: '00000000000000000008',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Typography',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: class or enum',
                      {
                        token: 'AUTO332',
                        index: 8,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Typography',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Typologies',
                  kind: 9,
                  detail: 'module "./type-exports.js"',
                  sortText: '00000000000000000009',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Typologies.',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type -> qualified type',
                      {
                        token: 'AUTO332',
                        index: 9,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Typologies',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Tyrant',
                  kind: 6,
                  detail: 'type Tyrant = string',
                  sortText: '00000000000000000010',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Tyrant',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        token: 'AUTO332',
                        index: 10,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Tyrant',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'any',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000011',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'any',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 11,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'any',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'bigint',
                  kind: 6,
                  detail: 'bigint',
                  sortText: '00000000000000000012',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'bigint',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 12,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'bigint',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'boolean',
                  kind: 6,
                  detail: 'boolean',
                  sortText: '00000000000000000013',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'boolean',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 13,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'boolean',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'empty',
                  kind: 6,
                  detail: 'empty',
                  sortText: '00000000000000000014',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'empty',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 14,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'empty',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'false',
                  kind: 6,
                  detail: 'false',
                  sortText: '00000000000000000015',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'false',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 15,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'false',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'mixed',
                  kind: 6,
                  detail: 'mixed',
                  sortText: '00000000000000000016',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'mixed',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 16,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'mixed',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'null',
                  kind: 6,
                  detail: 'null',
                  sortText: '00000000000000000017',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'null',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 17,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'null',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'number',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000018',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'number',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 18,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'number',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'string',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000019',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'string',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 19,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'string',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'symbol',
                  kind: 6,
                  detail: 'symbol',
                  sortText: '00000000000000000020',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'symbol',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 20,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'symbol',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'true',
                  kind: 6,
                  detail: 'true',
                  sortText: '00000000000000000021',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
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
                        token: 'AUTO332',
                        index: 21,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'true',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'void',
                  kind: 6,
                  detail: 'void',
                  sortText: '00000000000000000022',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'void',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 22,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'void',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$Call',
                  kind: 3,
                  detail: '$Call',
                  sortText: '00000000000000000023',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: '$Call',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 23,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Call',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$CharSet',
                  kind: 3,
                  detail: '$CharSet',
                  sortText: '00000000000000000024',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: '$CharSet',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 24,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$CharSet',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$Diff',
                  kind: 3,
                  detail: '$Diff',
                  sortText: '00000000000000000025',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: '$Diff',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 25,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Diff',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$ElementType',
                  kind: 3,
                  detail: '$ElementType',
                  sortText: '00000000000000000026',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
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
                        token: 'AUTO332',
                        index: 26,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$ElementType',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$Exact',
                  kind: 3,
                  detail: '$Exact',
                  sortText: '00000000000000000027',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: '$Exact',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 27,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Exact',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$Exports',
                  kind: 3,
                  detail: '$Exports',
                  sortText: '00000000000000000028',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: '$Exports',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 28,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Exports',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$KeyMirror',
                  kind: 3,
                  detail: '$KeyMirror',
                  sortText: '00000000000000000029',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: '$KeyMirror',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 29,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$KeyMirror',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$Keys',
                  kind: 3,
                  detail: '$Keys',
                  sortText: '00000000000000000030',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: '$Keys',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 30,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Keys',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$NonMaybeType',
                  kind: 3,
                  detail: '$NonMaybeType',
                  sortText: '00000000000000000031',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
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
                        token: 'AUTO332',
                        index: 31,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$NonMaybeType',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$ObjMap',
                  kind: 3,
                  detail: '$ObjMap',
                  sortText: '00000000000000000032',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: '$ObjMap',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 32,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$ObjMap',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$ObjMapi',
                  kind: 3,
                  detail: '$ObjMapi',
                  sortText: '00000000000000000033',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: '$ObjMapi',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 33,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$ObjMapi',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$PropertyType',
                  kind: 3,
                  detail: '$PropertyType',
                  sortText: '00000000000000000034',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
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
                        token: 'AUTO332',
                        index: 34,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$PropertyType',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$ReadOnly',
                  kind: 3,
                  detail: '$ReadOnly',
                  sortText: '00000000000000000035',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: '$ReadOnly',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 35,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$ReadOnly',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$Rest',
                  kind: 3,
                  detail: '$Rest',
                  sortText: '00000000000000000036',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: '$Rest',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 36,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Rest',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$TupleMap',
                  kind: 3,
                  detail: '$TupleMap',
                  sortText: '00000000000000000037',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
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
                        token: 'AUTO332',
                        index: 37,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$TupleMap',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: '$Values',
                  kind: 3,
                  detail: '$Values',
                  sortText: '00000000000000000038',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: '$Values',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 38,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Values',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Class',
                  kind: 3,
                  detail: 'Class',
                  sortText: '00000000000000000039',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Class',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 39,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Class',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Partial',
                  kind: 3,
                  detail: 'Partial',
                  sortText: '00000000000000000040',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Partial',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 40,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Partial',
                        ac_type: 'Actype',
                      },
                    ],
                  },
                },
                {
                  label: 'Required',
                  kind: 3,
                  detail: 'Required',
                  sortText: '00000000000000000041',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 27,
                        character: 18,
                      },
                      end: {
                        line: 27,
                        character: 18,
                      },
                    },
                    newText: 'Required',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 41,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Required',
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
    test('textDocument/completion invoked in jsx attribute with value', [
      addFile('jsx-attr-with-value.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx-attr-with-value.js'},
        position: {line: 9, character: 4},
        context: {triggerKind: 1},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: 'aaaa',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 3,
                      },
                      end: {
                        line: 9,
                        character: 4,
                      },
                    },
                    newText: 'aaaa',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'jsx attribute',
                      {
                        token: 'aAUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'aaaa',
                        ac_type: 'Acjsx',
                      },
                    ],
                  },
                },
                {
                  label: 'aaab',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 9,
                        character: 3,
                      },
                      end: {
                        line: 9,
                        character: 4,
                      },
                    },
                    newText: 'aaab',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'jsx attribute',
                      {
                        token: 'aAUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 1,
                        completion: 'aaab',
                        ac_type: 'Acjsx',
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
    test('textDocument/completion triggered by `[` - expression', [
      addFile('bracket.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/bracket.js'},
        position: {line: 12, character: 12},
        context: {triggerKind: 2, triggerCharacter: '['},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: '"a"',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 12,
                        character: 12,
                      },
                      end: {
                        line: 12,
                        character: 12,
                      },
                    },
                    newText: '"a"',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'bracket syntax member',
                      {
                        token: 'AUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '"a"',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '"b"',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 12,
                        character: 12,
                      },
                      end: {
                        line: 12,
                        character: 12,
                      },
                    },
                    newText: '"b"',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'bracket syntax member',
                      {
                        token: 'AUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '"b"',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'a',
                  kind: 6,
                  detail: 'empty',
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 12,
                        character: 12,
                      },
                      end: {
                        line: 12,
                        character: 12,
                      },
                    },
                    newText: 'a',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'a',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'o',
                  kind: 6,
                  detail: '{|a: number, b: string|}',
                  sortText: '00000000000000000003',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 12,
                        character: 12,
                      },
                      end: {
                        line: 12,
                        character: 12,
                      },
                    },
                    newText: 'o',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        token: 'AUTO332',
                        index: 3,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'o',
                        ac_type: 'Acmem',
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
    test('textDocument/completion triggered by `[` - type (indexed access)', [
      addFile('bracket.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/bracket.js'},
        position: {line: 14, character: 11},
        context: {triggerKind: 2, triggerCharacter: '['},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/completion',
            result: {
              isIncomplete: false,
              items: [
                {
                  label: '"bar"',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000000',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '"bar"',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'bracket syntax member',
                      {
                        token: 'AUTO332',
                        index: 0,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '"bar"',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '"foo"',
                  kind: 6,
                  detail: 'boolean',
                  sortText: '00000000000000000001',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '"foo"',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'bracket syntax member',
                      {
                        token: 'AUTO332',
                        index: 1,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '"foo"',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'B',
                  kind: 6,
                  detail: 'type B = T[any]',
                  sortText: '00000000000000000002',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'B',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        token: 'AUTO332',
                        index: 2,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'B',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'T',
                  kind: 6,
                  detail: 'type T = {|bar: string, foo: boolean|}',
                  sortText: '00000000000000000003',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'T',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        token: 'AUTO332',
                        index: 3,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'T',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'any',
                  kind: 6,
                  detail: 'any',
                  sortText: '00000000000000000004',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'any',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 4,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'any',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'bigint',
                  kind: 6,
                  detail: 'bigint',
                  sortText: '00000000000000000005',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'bigint',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 5,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'bigint',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'boolean',
                  kind: 6,
                  detail: 'boolean',
                  sortText: '00000000000000000006',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'boolean',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 6,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'boolean',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'empty',
                  kind: 6,
                  detail: 'empty',
                  sortText: '00000000000000000007',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'empty',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 7,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'empty',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'false',
                  kind: 6,
                  detail: 'false',
                  sortText: '00000000000000000008',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'false',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 8,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'false',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'mixed',
                  kind: 6,
                  detail: 'mixed',
                  sortText: '00000000000000000009',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'mixed',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 9,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'mixed',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'null',
                  kind: 6,
                  detail: 'null',
                  sortText: '00000000000000000010',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'null',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 10,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'null',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'number',
                  kind: 6,
                  detail: 'number',
                  sortText: '00000000000000000011',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'number',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 11,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'number',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'string',
                  kind: 6,
                  detail: 'string',
                  sortText: '00000000000000000012',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'string',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 12,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'string',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'symbol',
                  kind: 6,
                  detail: 'symbol',
                  sortText: '00000000000000000013',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'symbol',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 13,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'symbol',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'true',
                  kind: 6,
                  detail: 'true',
                  sortText: '00000000000000000014',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
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
                        token: 'AUTO332',
                        index: 14,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'true',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'void',
                  kind: 6,
                  detail: 'void',
                  sortText: '00000000000000000015',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'void',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 15,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'void',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$Call',
                  kind: 3,
                  detail: '$Call',
                  sortText: '00000000000000000016',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '$Call',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 16,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Call',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$CharSet',
                  kind: 3,
                  detail: '$CharSet',
                  sortText: '00000000000000000017',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '$CharSet',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 17,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$CharSet',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$Diff',
                  kind: 3,
                  detail: '$Diff',
                  sortText: '00000000000000000018',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '$Diff',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 18,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Diff',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$ElementType',
                  kind: 3,
                  detail: '$ElementType',
                  sortText: '00000000000000000019',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
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
                        token: 'AUTO332',
                        index: 19,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$ElementType',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$Exact',
                  kind: 3,
                  detail: '$Exact',
                  sortText: '00000000000000000020',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '$Exact',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 20,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Exact',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$Exports',
                  kind: 3,
                  detail: '$Exports',
                  sortText: '00000000000000000021',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '$Exports',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 21,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Exports',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$KeyMirror',
                  kind: 3,
                  detail: '$KeyMirror',
                  sortText: '00000000000000000022',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '$KeyMirror',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 22,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$KeyMirror',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$Keys',
                  kind: 3,
                  detail: '$Keys',
                  sortText: '00000000000000000023',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '$Keys',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 23,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Keys',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$NonMaybeType',
                  kind: 3,
                  detail: '$NonMaybeType',
                  sortText: '00000000000000000024',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
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
                        token: 'AUTO332',
                        index: 24,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$NonMaybeType',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$ObjMap',
                  kind: 3,
                  detail: '$ObjMap',
                  sortText: '00000000000000000025',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '$ObjMap',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 25,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$ObjMap',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$ObjMapi',
                  kind: 3,
                  detail: '$ObjMapi',
                  sortText: '00000000000000000026',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '$ObjMapi',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 26,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$ObjMapi',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$PropertyType',
                  kind: 3,
                  detail: '$PropertyType',
                  sortText: '00000000000000000027',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
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
                        token: 'AUTO332',
                        index: 27,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$PropertyType',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$ReadOnly',
                  kind: 3,
                  detail: '$ReadOnly',
                  sortText: '00000000000000000028',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '$ReadOnly',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 28,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$ReadOnly',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$Rest',
                  kind: 3,
                  detail: '$Rest',
                  sortText: '00000000000000000029',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '$Rest',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 29,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Rest',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$TupleMap',
                  kind: 3,
                  detail: '$TupleMap',
                  sortText: '00000000000000000030',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
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
                        token: 'AUTO332',
                        index: 30,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$TupleMap',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: '$Values',
                  kind: 3,
                  detail: '$Values',
                  sortText: '00000000000000000031',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: '$Values',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 31,
                        session_requests: 1,
                        typed_length: 0,
                        completion: '$Values',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'Class',
                  kind: 3,
                  detail: 'Class',
                  sortText: '00000000000000000032',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'Class',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 32,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Class',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'Partial',
                  kind: 3,
                  detail: 'Partial',
                  sortText: '00000000000000000033',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'Partial',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 33,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Partial',
                        ac_type: 'Acmem',
                      },
                    ],
                  },
                },
                {
                  label: 'Required',
                  kind: 3,
                  detail: 'Required',
                  sortText: '00000000000000000034',
                  insertTextFormat: 1,
                  textEdit: {
                    range: {
                      start: {
                        line: 14,
                        character: 11,
                      },
                      end: {
                        line: 14,
                        character: 11,
                      },
                    },
                    newText: 'Required',
                  },
                  command: {
                    title: '',
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        token: 'AUTO332',
                        index: 34,
                        session_requests: 1,
                        typed_length: 0,
                        completion: 'Required',
                        ac_type: 'Acmem',
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
): SuiteType);
