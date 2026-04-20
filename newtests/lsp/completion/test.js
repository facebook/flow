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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'a',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'a',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'a',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'async',
                        index: 1,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'async',
                  sortText: '00000000000000000001',
                  textEdit: {
                    newText: 'async',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'await',
                        index: 2,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'await',
                  sortText: '00000000000000000002',
                  textEdit: {
                    newText: 'await',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'b',
                        index: 3,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'string',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'b',
                  sortText: '00000000000000000003',
                  textEdit: {
                    newText: 'b',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'break',
                        index: 4,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'break',
                  sortText: '00000000000000000004',
                  textEdit: {
                    newText: 'break',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'case',
                        index: 5,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'case',
                  sortText: '00000000000000000005',
                  textEdit: {
                    newText: 'case',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'catch',
                        index: 6,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'catch',
                  sortText: '00000000000000000006',
                  textEdit: {
                    newText: 'catch',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'class',
                        index: 7,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'class',
                  sortText: '00000000000000000007',
                  textEdit: {
                    newText: 'class',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'const',
                        index: 8,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'const',
                  sortText: '00000000000000000008',
                  textEdit: {
                    newText: 'const',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'continue',
                        index: 9,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'continue',
                  sortText: '00000000000000000009',
                  textEdit: {
                    newText: 'continue',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'debugger',
                        index: 10,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'debugger',
                  sortText: '00000000000000000010',
                  textEdit: {
                    newText: 'debugger',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'declare',
                        index: 11,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'declare',
                  sortText: '00000000000000000011',
                  textEdit: {
                    newText: 'declare',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'default',
                        index: 12,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'default',
                  sortText: '00000000000000000012',
                  textEdit: {
                    newText: 'default',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'delete',
                        index: 13,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'delete',
                  sortText: '00000000000000000013',
                  textEdit: {
                    newText: 'delete',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'do',
                        index: 14,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'do',
                  sortText: '00000000000000000014',
                  textEdit: {
                    newText: 'do',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'else',
                        index: 15,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'else',
                  sortText: '00000000000000000015',
                  textEdit: {
                    newText: 'else',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'enum',
                        index: 16,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'enum',
                  sortText: '00000000000000000016',
                  textEdit: {
                    newText: 'enum',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'export',
                        index: 17,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'export',
                  sortText: '00000000000000000017',
                  textEdit: {
                    newText: 'export',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'extends',
                        index: 18,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'extends',
                  sortText: '00000000000000000018',
                  textEdit: {
                    newText: 'extends',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'finally',
                        index: 19,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'finally',
                  sortText: '00000000000000000019',
                  textEdit: {
                    newText: 'finally',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'for',
                        index: 20,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'for',
                  sortText: '00000000000000000020',
                  textEdit: {
                    newText: 'for',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'fred',
                        index: 21,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '(a: number, b: string) => number',
                  documentation: {
                    kind: 'markdown',
                    value:
                      "Docblock for 'fred'\n\n**@return**  {number} Docblock for return",
                  },
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'fred',
                  sortText: '00000000000000000021',
                  textEdit: {
                    newText: 'fred',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'function',
                        index: 22,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'function',
                  sortText: '00000000000000000022',
                  textEdit: {
                    newText: 'function',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'if',
                        index: 23,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'if',
                  sortText: '00000000000000000023',
                  textEdit: {
                    newText: 'if',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'implements',
                        index: 24,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'implements',
                  sortText: '00000000000000000024',
                  textEdit: {
                    newText: 'implements',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'import',
                        index: 25,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'import',
                  sortText: '00000000000000000025',
                  textEdit: {
                    newText: 'import',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'interface',
                        index: 26,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'interface',
                  sortText: '00000000000000000026',
                  textEdit: {
                    newText: 'interface',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'let',
                        index: 27,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'let',
                  sortText: '00000000000000000027',
                  textEdit: {
                    newText: 'let',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'new',
                        index: 28,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'new',
                  sortText: '00000000000000000028',
                  textEdit: {
                    newText: 'new',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'opaque',
                        index: 29,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'opaque',
                  sortText: '00000000000000000029',
                  textEdit: {
                    newText: 'opaque',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'return',
                        index: 30,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'return',
                  sortText: '00000000000000000030',
                  textEdit: {
                    newText: 'return',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'static',
                        index: 31,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'static',
                  sortText: '00000000000000000031',
                  textEdit: {
                    newText: 'static',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'switch',
                        index: 32,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'switch',
                  sortText: '00000000000000000032',
                  textEdit: {
                    newText: 'switch',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'this',
                      {
                        ac_type: 'Acid',
                        completion: 'this',
                        index: 33,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'this',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'this',
                  sortText: '00000000000000000033',
                  textEdit: {
                    newText: 'this',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'throw',
                        index: 34,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'throw',
                  sortText: '00000000000000000034',
                  textEdit: {
                    newText: 'throw',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'try',
                        index: 35,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'try',
                  sortText: '00000000000000000035',
                  textEdit: {
                    newText: 'try',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'type',
                        index: 36,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'type',
                  sortText: '00000000000000000036',
                  textEdit: {
                    newText: 'type',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'typeof',
                        index: 37,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'typeof',
                  sortText: '00000000000000000037',
                  textEdit: {
                    newText: 'typeof',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'var',
                        index: 38,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'var',
                  sortText: '00000000000000000038',
                  textEdit: {
                    newText: 'var',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'void',
                        index: 39,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'void',
                  sortText: '00000000000000000039',
                  textEdit: {
                    newText: 'void',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'while',
                        index: 40,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'while',
                  sortText: '00000000000000000040',
                  textEdit: {
                    newText: 'while',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'x',
                        index: 41,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '15',
                  insertTextFormat: 1,
                  kind: 12,
                  label: 'x',
                  sortText: '00000000000000000041',
                  textEdit: {
                    newText: 'x',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'yield',
                        index: 42,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'yield',
                  sortText: '00000000000000000042',
                  textEdit: {
                    newText: 'yield',
                    range: {
                      end: {
                        character: 15,
                        line: 10,
                      },
                      start: {
                        character: 15,
                        line: 10,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'test',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'string',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'test',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'test',
                    range: {
                      end: {
                        character: 2,
                        line: 6,
                      },
                      start: {
                        character: 2,
                        line: 6,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'test',
                        index: 0,
                        session_requests: 2,
                        token: 'tAUTO332',
                        typed_length: 1,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'string',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'test',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'test',
                    range: {
                      end: {
                        character: 3,
                        line: 6,
                      },
                      start: {
                        character: 2,
                        line: 6,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'test',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332te',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'string',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'test',
                  sortText: '00000000000000000000',
                  textEdit: {
                    insert: {
                      end: {
                        character: 2,
                        line: 7,
                      },
                      start: {
                        character: 2,
                        line: 7,
                      },
                    },
                    newText: 'test',
                    replace: {
                      end: {
                        character: 4,
                        line: 7,
                      },
                      start: {
                        character: 2,
                        line: 7,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'test',
                        index: 0,
                        session_requests: 2,
                        token: 'tAUTO332e',
                        typed_length: 1,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'string',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'test',
                  sortText: '00000000000000000000',
                  textEdit: {
                    insert: {
                      end: {
                        character: 3,
                        line: 7,
                      },
                      start: {
                        character: 2,
                        line: 7,
                      },
                    },
                    newText: 'test',
                    replace: {
                      end: {
                        character: 4,
                        line: 7,
                      },
                      start: {
                        character: 2,
                        line: 7,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'test',
                        index: 0,
                        session_requests: 3,
                        token: 'teAUTO332',
                        typed_length: 2,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'string',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'test',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'test',
                    range: {
                      end: {
                        character: 4,
                        line: 7,
                      },
                      start: {
                        character: 2,
                        line: 7,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'test',
                        index: 0,
                        session_requests: 1,
                        token: 'teAUTO332',
                        typed_length: 2,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'string',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'test',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'test',
                    range: {
                      end: {
                        character: 4,
                        line: 7,
                      },
                      start: {
                        character: 2,
                        line: 7,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'aClass',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'class aClass',
                  insertTextFormat: 1,
                  kind: 7,
                  label: 'aClass',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'aClass',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'aFunction',
                        index: 1,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '() => null',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'aFunction',
                  sortText: '00000000000000000001',
                  textEdit: {
                    newText: 'aFunction',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'aNumber',
                        index: 2,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'aNumber',
                  sortText: '00000000000000000002',
                  textEdit: {
                    newText: 'aNumber',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'async',
                        index: 3,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'async',
                  sortText: '00000000000000000003',
                  textEdit: {
                    newText: 'async',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'await',
                        index: 4,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'await',
                  sortText: '00000000000000000004',
                  textEdit: {
                    newText: 'await',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'break',
                        index: 5,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'break',
                  sortText: '00000000000000000005',
                  textEdit: {
                    newText: 'break',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'case',
                        index: 6,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'case',
                  sortText: '00000000000000000006',
                  textEdit: {
                    newText: 'case',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'catch',
                        index: 7,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'catch',
                  sortText: '00000000000000000007',
                  textEdit: {
                    newText: 'catch',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'class',
                        index: 8,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'class',
                  sortText: '00000000000000000008',
                  textEdit: {
                    newText: 'class',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'const',
                        index: 9,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'const',
                  sortText: '00000000000000000009',
                  textEdit: {
                    newText: 'const',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'continue',
                        index: 10,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'continue',
                  sortText: '00000000000000000010',
                  textEdit: {
                    newText: 'continue',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'debugger',
                        index: 11,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'debugger',
                  sortText: '00000000000000000011',
                  textEdit: {
                    newText: 'debugger',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'declare',
                        index: 12,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'declare',
                  sortText: '00000000000000000012',
                  textEdit: {
                    newText: 'declare',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'default',
                        index: 13,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'default',
                  sortText: '00000000000000000013',
                  textEdit: {
                    newText: 'default',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'delete',
                        index: 14,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'delete',
                  sortText: '00000000000000000014',
                  textEdit: {
                    newText: 'delete',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'do',
                        index: 15,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'do',
                  sortText: '00000000000000000015',
                  textEdit: {
                    newText: 'do',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'else',
                        index: 16,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'else',
                  sortText: '00000000000000000016',
                  textEdit: {
                    newText: 'else',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'enum',
                        index: 17,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'enum',
                  sortText: '00000000000000000017',
                  textEdit: {
                    newText: 'enum',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'export',
                        index: 18,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'export',
                  sortText: '00000000000000000018',
                  textEdit: {
                    newText: 'export',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'extends',
                        index: 19,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'extends',
                  sortText: '00000000000000000019',
                  textEdit: {
                    newText: 'extends',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'finally',
                        index: 20,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'finally',
                  sortText: '00000000000000000020',
                  textEdit: {
                    newText: 'finally',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'foo',
                        index: 21,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '() => void',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'foo',
                  sortText: '00000000000000000021',
                  textEdit: {
                    newText: 'foo',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'for',
                        index: 22,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'for',
                  sortText: '00000000000000000022',
                  textEdit: {
                    newText: 'for',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'function',
                        index: 23,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'function',
                  sortText: '00000000000000000023',
                  textEdit: {
                    newText: 'function',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'if',
                        index: 24,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'if',
                  sortText: '00000000000000000024',
                  textEdit: {
                    newText: 'if',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'implements',
                        index: 25,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'implements',
                  sortText: '00000000000000000025',
                  textEdit: {
                    newText: 'implements',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'import',
                        index: 26,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'import',
                  sortText: '00000000000000000026',
                  textEdit: {
                    newText: 'import',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'interface',
                        index: 27,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'interface',
                  sortText: '00000000000000000027',
                  textEdit: {
                    newText: 'interface',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'let',
                        index: 28,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'let',
                  sortText: '00000000000000000028',
                  textEdit: {
                    newText: 'let',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'new',
                        index: 29,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'new',
                  sortText: '00000000000000000029',
                  textEdit: {
                    newText: 'new',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'opaque',
                        index: 30,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'opaque',
                  sortText: '00000000000000000030',
                  textEdit: {
                    newText: 'opaque',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'return',
                        index: 31,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'return',
                  sortText: '00000000000000000031',
                  textEdit: {
                    newText: 'return',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'static',
                        index: 32,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'static',
                  sortText: '00000000000000000032',
                  textEdit: {
                    newText: 'static',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'switch',
                        index: 33,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'switch',
                  sortText: '00000000000000000033',
                  textEdit: {
                    newText: 'switch',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'this',
                      {
                        ac_type: 'Acid',
                        completion: 'this',
                        index: 34,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'this',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'this',
                  sortText: '00000000000000000034',
                  textEdit: {
                    newText: 'this',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'throw',
                        index: 35,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'throw',
                  sortText: '00000000000000000035',
                  textEdit: {
                    newText: 'throw',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'try',
                        index: 36,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'try',
                  sortText: '00000000000000000036',
                  textEdit: {
                    newText: 'try',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'type',
                        index: 37,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'type',
                  sortText: '00000000000000000037',
                  textEdit: {
                    newText: 'type',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'typeof',
                        index: 38,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'typeof',
                  sortText: '00000000000000000038',
                  textEdit: {
                    newText: 'typeof',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'var',
                        index: 39,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'var',
                  sortText: '00000000000000000039',
                  textEdit: {
                    newText: 'var',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'void',
                        index: 40,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'void',
                  sortText: '00000000000000000040',
                  textEdit: {
                    newText: 'void',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'while',
                        index: 41,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'while',
                  sortText: '00000000000000000041',
                  textEdit: {
                    newText: 'while',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'x',
                        index: 42,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '15',
                  insertTextFormat: 1,
                  kind: 12,
                  label: 'x',
                  sortText: '00000000000000000042',
                  textEdit: {
                    newText: 'x',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'yield',
                        index: 43,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'yield',
                  sortText: '00000000000000000043',
                  textEdit: {
                    newText: 'yield',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 15,
                        line: 13,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'aFunction',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '(arg1: number, arg2: string) => null',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'aFunction',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'aFunction',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'async',
                        index: 1,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'async',
                  sortText: '00000000000000000001',
                  textEdit: {
                    newText: 'async',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'await',
                        index: 2,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'await',
                  sortText: '00000000000000000002',
                  textEdit: {
                    newText: 'await',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'break',
                        index: 3,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'break',
                  sortText: '00000000000000000003',
                  textEdit: {
                    newText: 'break',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'case',
                        index: 4,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'case',
                  sortText: '00000000000000000004',
                  textEdit: {
                    newText: 'case',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'catch',
                        index: 5,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'catch',
                  sortText: '00000000000000000005',
                  textEdit: {
                    newText: 'catch',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'class',
                        index: 6,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'class',
                  sortText: '00000000000000000006',
                  textEdit: {
                    newText: 'class',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'const',
                        index: 7,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'const',
                  sortText: '00000000000000000007',
                  textEdit: {
                    newText: 'const',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'continue',
                        index: 8,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'continue',
                  sortText: '00000000000000000008',
                  textEdit: {
                    newText: 'continue',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'debugger',
                        index: 9,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'debugger',
                  sortText: '00000000000000000009',
                  textEdit: {
                    newText: 'debugger',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'declare',
                        index: 10,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'declare',
                  sortText: '00000000000000000010',
                  textEdit: {
                    newText: 'declare',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'default',
                        index: 11,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'default',
                  sortText: '00000000000000000011',
                  textEdit: {
                    newText: 'default',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'delete',
                        index: 12,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'delete',
                  sortText: '00000000000000000012',
                  textEdit: {
                    newText: 'delete',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'do',
                        index: 13,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'do',
                  sortText: '00000000000000000013',
                  textEdit: {
                    newText: 'do',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'else',
                        index: 14,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'else',
                  sortText: '00000000000000000014',
                  textEdit: {
                    newText: 'else',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'enum',
                        index: 15,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'enum',
                  sortText: '00000000000000000015',
                  textEdit: {
                    newText: 'enum',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'export',
                        index: 16,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'export',
                  sortText: '00000000000000000016',
                  textEdit: {
                    newText: 'export',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'extends',
                        index: 17,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'extends',
                  sortText: '00000000000000000017',
                  textEdit: {
                    newText: 'extends',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'finally',
                        index: 18,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'finally',
                  sortText: '00000000000000000018',
                  textEdit: {
                    newText: 'finally',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'foo',
                        index: 19,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '() => void',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'foo',
                  sortText: '00000000000000000019',
                  textEdit: {
                    newText: 'foo',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'for',
                        index: 20,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'for',
                  sortText: '00000000000000000020',
                  textEdit: {
                    newText: 'for',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'function',
                        index: 21,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'function',
                  sortText: '00000000000000000021',
                  textEdit: {
                    newText: 'function',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'if',
                        index: 22,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'if',
                  sortText: '00000000000000000022',
                  textEdit: {
                    newText: 'if',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'implements',
                        index: 23,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'implements',
                  sortText: '00000000000000000023',
                  textEdit: {
                    newText: 'implements',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'import',
                        index: 24,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'import',
                  sortText: '00000000000000000024',
                  textEdit: {
                    newText: 'import',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'interface',
                        index: 25,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'interface',
                  sortText: '00000000000000000025',
                  textEdit: {
                    newText: 'interface',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'let',
                        index: 26,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'let',
                  sortText: '00000000000000000026',
                  textEdit: {
                    newText: 'let',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'new',
                        index: 27,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'new',
                  sortText: '00000000000000000027',
                  textEdit: {
                    newText: 'new',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'opaque',
                        index: 28,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'opaque',
                  sortText: '00000000000000000028',
                  textEdit: {
                    newText: 'opaque',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'return',
                        index: 29,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'return',
                  sortText: '00000000000000000029',
                  textEdit: {
                    newText: 'return',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'static',
                        index: 30,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'static',
                  sortText: '00000000000000000030',
                  textEdit: {
                    newText: 'static',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'switch',
                        index: 31,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'switch',
                  sortText: '00000000000000000031',
                  textEdit: {
                    newText: 'switch',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'this',
                      {
                        ac_type: 'Acid',
                        completion: 'this',
                        index: 32,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'this',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'this',
                  sortText: '00000000000000000032',
                  textEdit: {
                    newText: 'this',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'throw',
                        index: 33,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'throw',
                  sortText: '00000000000000000033',
                  textEdit: {
                    newText: 'throw',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'try',
                        index: 34,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'try',
                  sortText: '00000000000000000034',
                  textEdit: {
                    newText: 'try',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'type',
                        index: 35,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'type',
                  sortText: '00000000000000000035',
                  textEdit: {
                    newText: 'type',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'typeof',
                        index: 36,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'typeof',
                  sortText: '00000000000000000036',
                  textEdit: {
                    newText: 'typeof',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'var',
                        index: 37,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'var',
                  sortText: '00000000000000000037',
                  textEdit: {
                    newText: 'var',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'void',
                        index: 38,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'void',
                  sortText: '00000000000000000038',
                  textEdit: {
                    newText: 'void',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'while',
                        index: 39,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'while',
                  sortText: '00000000000000000039',
                  textEdit: {
                    newText: 'while',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'x',
                        index: 40,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '15',
                  insertTextFormat: 1,
                  kind: 12,
                  label: 'x',
                  sortText: '00000000000000000040',
                  textEdit: {
                    newText: 'x',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'yield',
                        index: 41,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'yield',
                  sortText: '00000000000000000041',
                  textEdit: {
                    newText: 'yield',
                    range: {
                      end: {
                        character: 15,
                        line: 9,
                      },
                      start: {
                        character: 15,
                        line: 9,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'jsx attribute',
                      {
                        ac_type: 'Acjsx',
                        completion: 'a',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'a',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'a=',
                    range: {
                      end: {
                        character: 4,
                        line: 12,
                      },
                      start: {
                        character: 4,
                        line: 12,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'jsx attribute',
                      {
                        ac_type: 'Acjsx',
                        completion: 'ref',
                        index: 1,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'RefSetter<C> | void',
                  insertTextFormat: 1,
                  kind: 13,
                  label: 'ref',
                  sortText: '00000000000000000001',
                  textEdit: {
                    newText: 'ref=',
                    range: {
                      end: {
                        character: 4,
                        line: 12,
                      },
                      start: {
                        character: 4,
                        line: 12,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'async',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'async',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'async',
                    range: {
                      end: {
                        character: 1,
                        line: 11,
                      },
                      start: {
                        character: 1,
                        line: 11,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'await',
                        index: 1,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'await',
                  sortText: '00000000000000000001',
                  textEdit: {
                    newText: 'await',
                    range: {
                      end: {
                        character: 1,
                        line: 11,
                      },
                      start: {
                        character: 1,
                        line: 11,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'C',
                        index: 2,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'class C',
                  insertTextFormat: 1,
                  kind: 7,
                  label: 'C',
                  sortText: '00000000000000000002',
                  textEdit: {
                    newText: 'C',
                    range: {
                      end: {
                        character: 1,
                        line: 11,
                      },
                      start: {
                        character: 1,
                        line: 11,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'class',
                        index: 3,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'class',
                  sortText: '00000000000000000003',
                  textEdit: {
                    newText: 'class',
                    range: {
                      end: {
                        character: 1,
                        line: 11,
                      },
                      start: {
                        character: 1,
                        line: 11,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'D',
                        index: 4,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '(props: Props) => void',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'D',
                  sortText: '00000000000000000004',
                  textEdit: {
                    newText: 'D',
                    range: {
                      end: {
                        character: 1,
                        line: 11,
                      },
                      start: {
                        character: 1,
                        line: 11,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'delete',
                        index: 5,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'delete',
                  sortText: '00000000000000000005',
                  textEdit: {
                    newText: 'delete',
                    range: {
                      end: {
                        character: 1,
                        line: 11,
                      },
                      start: {
                        character: 1,
                        line: 11,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'function',
                        index: 6,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'function',
                  sortText: '00000000000000000006',
                  textEdit: {
                    newText: 'function',
                    range: {
                      end: {
                        character: 1,
                        line: 11,
                      },
                      start: {
                        character: 1,
                        line: 11,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'import',
                        index: 7,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'import',
                  sortText: '00000000000000000007',
                  textEdit: {
                    newText: 'import',
                    range: {
                      end: {
                        character: 1,
                        line: 11,
                      },
                      start: {
                        character: 1,
                        line: 11,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'new',
                        index: 8,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'new',
                  sortText: '00000000000000000008',
                  textEdit: {
                    newText: 'new',
                    range: {
                      end: {
                        character: 1,
                        line: 11,
                      },
                      start: {
                        character: 1,
                        line: 11,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acid',
                        completion: 'React',
                        index: 9,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'module "react"',
                  insertTextFormat: 1,
                  kind: 9,
                  label: 'React',
                  sortText: '00000000000000000009',
                  textEdit: {
                    newText: 'React',
                    range: {
                      end: {
                        character: 1,
                        line: 11,
                      },
                      start: {
                        character: 1,
                        line: 11,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'typeof',
                        index: 10,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'typeof',
                  sortText: '00000000000000000010',
                  textEdit: {
                    newText: 'typeof',
                    range: {
                      end: {
                        character: 1,
                        line: 11,
                      },
                      start: {
                        character: 1,
                        line: 11,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'void',
                        index: 11,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'void',
                  sortText: '00000000000000000011',
                  textEdit: {
                    newText: 'void',
                    range: {
                      end: {
                        character: 1,
                        line: 11,
                      },
                      start: {
                        character: 1,
                        line: 11,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'keyword',
                      {
                        ac_type: 'Acid',
                        completion: 'yield',
                        index: 12,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'yield',
                  sortText: '00000000000000000012',
                  textEdit: {
                    newText: 'yield',
                    range: {
                      end: {
                        character: 1,
                        line: 11,
                      },
                      start: {
                        character: 1,
                        line: 11,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'jsx attribute',
                      {
                        ac_type: 'Acjsx',
                        completion: 'a',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'a',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'a=',
                    range: {
                      end: {
                        character: 4,
                        line: 12,
                      },
                      start: {
                        character: 4,
                        line: 12,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'jsx attribute',
                      {
                        ac_type: 'Acjsx',
                        completion: 'ref',
                        index: 1,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'RefSetter<C> | void',
                  insertTextFormat: 1,
                  kind: 13,
                  label: 'ref',
                  sortText: '00000000000000000001',
                  textEdit: {
                    newText: 'ref=',
                    range: {
                      end: {
                        character: 4,
                        line: 12,
                      },
                      start: {
                        character: 4,
                        line: 12,
                      },
                    },
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
                    command: {
                      arguments: [
                        'textDocument/completion',
                        'jsx attribute',
                        {
                          ac_type: 'Acjsx',
                          completion: 'a',
                          index: 0,
                          session_requests: 1,
                          token: 'AUTO332',
                          typed_length: 0,
                        },
                      ],
                      command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                      title: '',
                    },
                    detail: 'number',
                    insertTextFormat: 1,
                    kind: 6,
                    label: 'a',
                    sortText: '00000000000000000000',
                    textEdit: {
                      newText: 'a=',
                      range: {
                        end: {
                          character: 4,
                          line: 13,
                        },
                        start: {
                          character: 4,
                          line: 13,
                        },
                      },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'jsx attribute',
                      {
                        ac_type: 'Acjsx',
                        completion: 'a',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'a',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'a=',
                    range: {
                      end: {
                        character: 4,
                        line: 13,
                      },
                      start: {
                        character: 4,
                        line: 13,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'childContextTypes',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'empty',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'childContextTypes',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'childContextTypes',
                    range: {
                      end: {
                        character: 3,
                        line: 14,
                      },
                      start: {
                        character: 3,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'contextTypes',
                        index: 1,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'empty',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'contextTypes',
                  sortText: '00000000000000000001',
                  textEdit: {
                    newText: 'contextTypes',
                    range: {
                      end: {
                        character: 3,
                        line: 14,
                      },
                      start: {
                        character: 3,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'displayName',
                        index: 2,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '(?string) | void',
                  insertTextFormat: 1,
                  kind: 13,
                  label: 'displayName',
                  sortText: '00000000000000000002',
                  textEdit: {
                    newText: 'displayName',
                    range: {
                      end: {
                        character: 3,
                        line: 14,
                      },
                      start: {
                        character: 3,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'propTypes',
                        index: 3,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'any',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'propTypes',
                  sortText: '00000000000000000003',
                  textEdit: {
                    newText: 'propTypes',
                    range: {
                      end: {
                        character: 3,
                        line: 14,
                      },
                      start: {
                        character: 3,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'apply',
                        index: 4,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail:
                    '<T, R, A: $ArrayLike<mixed> = Readonly<[]>>(thisArg: T, args?: A) => R',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'apply',
                  sortText: '00000000000000000004',
                  textEdit: {
                    newText: 'apply',
                    range: {
                      end: {
                        character: 3,
                        line: 14,
                      },
                      start: {
                        character: 3,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'arguments',
                        index: 5,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'any',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'arguments',
                  sortText: '00000000000000000005',
                  textEdit: {
                    newText: 'arguments',
                    range: {
                      end: {
                        character: 3,
                        line: 14,
                      },
                      start: {
                        character: 3,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'bind',
                        index: 6,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'bind',
                  sortText: '00000000000000000006',
                  textEdit: {
                    newText: 'bind',
                    range: {
                      end: {
                        character: 3,
                        line: 14,
                      },
                      start: {
                        character: 3,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'call',
                        index: 7,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail:
                    '<T, R, A: $ArrayLike<mixed> = Readonly<[]>>(thisArg: T, ...args: A) => R',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'call',
                  sortText: '00000000000000000007',
                  textEdit: {
                    newText: 'call',
                    range: {
                      end: {
                        character: 3,
                        line: 14,
                      },
                      start: {
                        character: 3,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'caller',
                        index: 8,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'any | null',
                  insertTextFormat: 1,
                  kind: 13,
                  label: 'caller',
                  sortText: '00000000000000000008',
                  textEdit: {
                    newText: 'caller',
                    range: {
                      end: {
                        character: 3,
                        line: 14,
                      },
                      start: {
                        character: 3,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'length',
                        index: 9,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'length',
                  sortText: '00000000000000000009',
                  textEdit: {
                    newText: 'length',
                    range: {
                      end: {
                        character: 3,
                        line: 14,
                      },
                      start: {
                        character: 3,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'name',
                        index: 10,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'string',
                  documentation: {
                    kind: 'markdown',
                    value: 'Returns the name of the function.',
                  },
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'name',
                  sortText: '00000000000000000010',
                  textEdit: {
                    newText: 'name',
                    range: {
                      end: {
                        character: 3,
                        line: 14,
                      },
                      start: {
                        character: 3,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'toString',
                        index: 11,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '() => string',
                  documentation: {
                    kind: 'markdown',
                    value: 'Returns a string representation of a function.',
                  },
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'toString',
                  sortText: '00000000000000000011',
                  textEdit: {
                    newText: 'toString',
                    range: {
                      end: {
                        character: 3,
                        line: 14,
                      },
                      start: {
                        character: 3,
                        line: 14,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'childContextTypes',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'empty',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'childContextTypes',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'childContextTypes',
                    range: {
                      end: {
                        character: 2,
                        line: 15,
                      },
                      start: {
                        character: 2,
                        line: 15,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'contextTypes',
                        index: 1,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'empty',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'contextTypes',
                  sortText: '00000000000000000001',
                  textEdit: {
                    newText: 'contextTypes',
                    range: {
                      end: {
                        character: 2,
                        line: 15,
                      },
                      start: {
                        character: 2,
                        line: 15,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'displayName',
                        index: 2,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '(?string) | void',
                  insertTextFormat: 1,
                  kind: 13,
                  label: 'displayName',
                  sortText: '00000000000000000002',
                  textEdit: {
                    newText: 'displayName',
                    range: {
                      end: {
                        character: 2,
                        line: 15,
                      },
                      start: {
                        character: 2,
                        line: 15,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'propTypes',
                        index: 3,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'any',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'propTypes',
                  sortText: '00000000000000000003',
                  textEdit: {
                    newText: 'propTypes',
                    range: {
                      end: {
                        character: 2,
                        line: 15,
                      },
                      start: {
                        character: 2,
                        line: 15,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'apply',
                        index: 4,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail:
                    '<T, R, A: $ArrayLike<mixed> = Readonly<[]>>(thisArg: T, args?: A) => R',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'apply',
                  sortText: '00000000000000000004',
                  textEdit: {
                    newText: 'apply',
                    range: {
                      end: {
                        character: 2,
                        line: 15,
                      },
                      start: {
                        character: 2,
                        line: 15,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'arguments',
                        index: 5,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'any',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'arguments',
                  sortText: '00000000000000000005',
                  textEdit: {
                    newText: 'arguments',
                    range: {
                      end: {
                        character: 2,
                        line: 15,
                      },
                      start: {
                        character: 2,
                        line: 15,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'bind',
                        index: 6,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '(thisArg: any, ...argArray: Array<any>) => any',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'bind',
                  sortText: '00000000000000000006',
                  textEdit: {
                    newText: 'bind',
                    range: {
                      end: {
                        character: 2,
                        line: 15,
                      },
                      start: {
                        character: 2,
                        line: 15,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'call',
                        index: 7,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail:
                    '<T, R, A: $ArrayLike<mixed> = Readonly<[]>>(thisArg: T, ...args: A) => R',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'call',
                  sortText: '00000000000000000007',
                  textEdit: {
                    newText: 'call',
                    range: {
                      end: {
                        character: 2,
                        line: 15,
                      },
                      start: {
                        character: 2,
                        line: 15,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'caller',
                        index: 8,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'any | null',
                  insertTextFormat: 1,
                  kind: 13,
                  label: 'caller',
                  sortText: '00000000000000000008',
                  textEdit: {
                    newText: 'caller',
                    range: {
                      end: {
                        character: 2,
                        line: 15,
                      },
                      start: {
                        character: 2,
                        line: 15,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'length',
                        index: 9,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'length',
                  sortText: '00000000000000000009',
                  textEdit: {
                    newText: 'length',
                    range: {
                      end: {
                        character: 2,
                        line: 15,
                      },
                      start: {
                        character: 2,
                        line: 15,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'name',
                        index: 10,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'string',
                  documentation: {
                    kind: 'markdown',
                    value: 'Returns the name of the function.',
                  },
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'name',
                  sortText: '00000000000000000010',
                  textEdit: {
                    newText: 'name',
                    range: {
                      end: {
                        character: 2,
                        line: 15,
                      },
                      start: {
                        character: 2,
                        line: 15,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'member',
                      {
                        ac_type: 'Acmem',
                        completion: 'toString',
                        index: 11,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '() => string',
                  documentation: {
                    kind: 'markdown',
                    value: 'Returns a string representation of a function.',
                  },
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'toString',
                  sortText: '00000000000000000011',
                  textEdit: {
                    newText: 'toString',
                    range: {
                      end: {
                        character: 2,
                        line: 15,
                      },
                      start: {
                        character: 2,
                        line: 15,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        ac_type: 'Actype',
                        completion: 'Tympanic',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'type Tympanic = number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'Tympanic',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'Tympanic',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'unqualified type parameter',
                      {
                        ac_type: 'Actype',
                        completion: 'Typaram',
                        index: 1,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'Typaram',
                  insertTextFormat: 1,
                  kind: 25,
                  label: 'Typaram',
                  sortText: '00000000000000000001',
                  textEdit: {
                    newText: 'Typaram',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'unqualified type -> qualified type',
                      {
                        ac_type: 'Actype',
                        completion: 'Types',
                        index: 2,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'module "./type-exports.js"',
                  insertTextFormat: 1,
                  kind: 9,
                  label: 'Types',
                  sortText: '00000000000000000002',
                  textEdit: {
                    newText: 'Types.',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        ac_type: 'Actype',
                        completion: 'Typesafe',
                        index: 3,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'interface Typesafe',
                  insertTextFormat: 1,
                  kind: 8,
                  label: 'Typesafe',
                  sortText: '00000000000000000003',
                  textEdit: {
                    newText: 'Typesafe',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        ac_type: 'Actype',
                        completion: 'Typeset',
                        index: 4,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'interface Typeset',
                  insertTextFormat: 1,
                  kind: 8,
                  label: 'Typeset',
                  sortText: '00000000000000000004',
                  textEdit: {
                    newText: 'Typeset',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: class, record, enum',
                      {
                        ac_type: 'Actype',
                        completion: 'Typewriter',
                        index: 5,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'class Typewriter',
                  insertTextFormat: 1,
                  kind: 7,
                  label: 'Typewriter',
                  sortText: '00000000000000000005',
                  textEdit: {
                    newText: 'Typewriter',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        ac_type: 'Actype',
                        completion: 'Typhoon',
                        index: 6,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'type Typhoon = string',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'Typhoon',
                  sortText: '00000000000000000006',
                  textEdit: {
                    newText: 'Typhoon',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        ac_type: 'Actype',
                        completion: 'Typnotism',
                        index: 7,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'type Typnotism = number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'Typnotism',
                  sortText: '00000000000000000007',
                  textEdit: {
                    newText: 'Typnotism',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: class, record, enum',
                      {
                        ac_type: 'Actype',
                        completion: 'Typography',
                        index: 8,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'class Typewriter',
                  insertTextFormat: 1,
                  kind: 7,
                  label: 'Typography',
                  sortText: '00000000000000000008',
                  textEdit: {
                    newText: 'Typography',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'unqualified type -> qualified type',
                      {
                        ac_type: 'Actype',
                        completion: 'Typologies',
                        index: 9,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'module "./type-exports.js"',
                  insertTextFormat: 1,
                  kind: 9,
                  label: 'Typologies',
                  sortText: '00000000000000000009',
                  textEdit: {
                    newText: 'Typologies.',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        ac_type: 'Actype',
                        completion: 'Tyrant',
                        index: 10,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'type Tyrant = string',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'Tyrant',
                  sortText: '00000000000000000010',
                  textEdit: {
                    newText: 'Tyrant',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type operators',
                      {
                        ac_type: 'Actype',
                        completion: 'component',
                        index: 11,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'component',
                  documentation: {
                    kind: 'markdown',
                    value:
                      '[component type](https://flow.org/en/docs/react/component-types/)',
                  },
                  insertTextFormat: 2,
                  kind: 14,
                  label: 'component',
                  sortText: '00000000000000000011',
                  textEdit: {
                    newText: 'component($1)',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type operators',
                      {
                        ac_type: 'Actype',
                        completion: 'hook',
                        index: 12,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'hook',
                  documentation: {
                    kind: 'markdown',
                    value:
                      '[hook type](https://flow.org/en/docs/react/hook-syntax/#hook-type-annotations)',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'hook',
                  sortText: '00000000000000000012',
                  textEdit: {
                    newText: 'hook ',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type operators',
                      {
                        ac_type: 'Actype',
                        completion: 'renders',
                        index: 13,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'renders',
                  documentation: {
                    kind: 'markdown',
                    value:
                      '`renders A` means that it will eventually render exactly one React element `A`. See https://flow.org/en/docs/react/render-types/ for more details.',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'renders',
                  sortText: '00000000000000000013',
                  textEdit: {
                    newText: 'renders ',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type operators',
                      {
                        ac_type: 'Actype',
                        completion: 'renders*',
                        index: 14,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'renders*',
                  documentation: {
                    kind: 'markdown',
                    value:
                      '`renders* A` means that it will eventually render any amount of `A`. See https://flow.org/en/docs/react/render-types/ for more details.',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'renders*',
                  sortText: '00000000000000000014',
                  textEdit: {
                    newText: 'renders* ',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type operators',
                      {
                        ac_type: 'Actype',
                        completion: 'renders?',
                        index: 15,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'renders?',
                  documentation: {
                    kind: 'markdown',
                    value:
                      '`renders? A` means that it will eventually render zero or one React element `A`. See https://flow.org/en/docs/react/render-types/ for more details.',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'renders?',
                  sortText: '00000000000000000015',
                  textEdit: {
                    newText: 'renders? ',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'any',
                        index: 16,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'any',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'any',
                  sortText: '00000000000000000016',
                  textEdit: {
                    newText: 'any',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'bigint',
                        index: 17,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'bigint',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'bigint',
                  sortText: '00000000000000000017',
                  textEdit: {
                    newText: 'bigint',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'boolean',
                        index: 18,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'boolean',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'boolean',
                  sortText: '00000000000000000018',
                  textEdit: {
                    newText: 'boolean',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'empty',
                        index: 19,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'empty',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'empty',
                  sortText: '00000000000000000019',
                  textEdit: {
                    newText: 'empty',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'false',
                        index: 20,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'false',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'false',
                  sortText: '00000000000000000020',
                  textEdit: {
                    newText: 'false',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'mixed',
                        index: 21,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'mixed',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'mixed',
                  sortText: '00000000000000000021',
                  textEdit: {
                    newText: 'mixed',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'null',
                        index: 22,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'null',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'null',
                  sortText: '00000000000000000022',
                  textEdit: {
                    newText: 'null',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'number',
                        index: 23,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'number',
                  sortText: '00000000000000000023',
                  textEdit: {
                    newText: 'number',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'string',
                        index: 24,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'string',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'string',
                  sortText: '00000000000000000024',
                  textEdit: {
                    newText: 'string',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'symbol',
                        index: 25,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'symbol',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'symbol',
                  sortText: '00000000000000000025',
                  textEdit: {
                    newText: 'symbol',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'true',
                        index: 26,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'true',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'true',
                  sortText: '00000000000000000026',
                  textEdit: {
                    newText: 'true',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'unknown',
                        index: 27,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'unknown',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'unknown',
                  sortText: '00000000000000000027',
                  textEdit: {
                    newText: 'unknown',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'void',
                        index: 28,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'void',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'void',
                  sortText: '00000000000000000028',
                  textEdit: {
                    newText: 'void',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: '$Exact',
                        index: 29,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$Exact',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$Exact',
                  sortText: '00000000000000000029',
                  textEdit: {
                    newText: '$Exact',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: '$Exports',
                        index: 30,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$Exports',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$Exports',
                  sortText: '00000000000000000030',
                  textEdit: {
                    newText: '$Exports',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: '$KeyMirror',
                        index: 31,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$KeyMirror',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$KeyMirror',
                  sortText: '00000000000000000031',
                  textEdit: {
                    newText: '$KeyMirror',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: '$Keys',
                        index: 32,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$Keys',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$Keys',
                  sortText: '00000000000000000032',
                  textEdit: {
                    newText: '$Keys',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: '$NonMaybeType',
                        index: 33,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$NonMaybeType',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$NonMaybeType',
                  sortText: '00000000000000000033',
                  textEdit: {
                    newText: '$NonMaybeType',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: '$ReadOnly',
                        index: 34,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$ReadOnly',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$ReadOnly',
                  sortText: '00000000000000000034',
                  textEdit: {
                    newText: '$ReadOnly',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: '$Values',
                        index: 35,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$Values',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$Values',
                  sortText: '00000000000000000035',
                  textEdit: {
                    newText: '$Values',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'Class',
                        index: 36,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'Class',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'Class',
                  sortText: '00000000000000000036',
                  textEdit: {
                    newText: 'Class',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'keyof',
                        index: 37,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'keyof',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'keyof',
                  sortText: '00000000000000000037',
                  textEdit: {
                    newText: 'keyof',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'NonNullable',
                        index: 38,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'NonNullable',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'NonNullable',
                  sortText: '00000000000000000038',
                  textEdit: {
                    newText: 'NonNullable',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'Partial',
                        index: 39,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'Partial',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'Partial',
                  sortText: '00000000000000000039',
                  textEdit: {
                    newText: 'Partial',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'Readonly',
                        index: 40,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'Readonly',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'Readonly',
                  sortText: '00000000000000000040',
                  textEdit: {
                    newText: 'Readonly',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'Required',
                        index: 41,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'Required',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'Required',
                  sortText: '00000000000000000041',
                  textEdit: {
                    newText: 'Required',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'StringPrefix',
                        index: 42,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'StringPrefix',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'StringPrefix',
                  sortText: '00000000000000000042',
                  textEdit: {
                    newText: 'StringPrefix',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'StringSuffix',
                        index: 43,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'StringSuffix',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'StringSuffix',
                  sortText: '00000000000000000043',
                  textEdit: {
                    newText: 'StringSuffix',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Actype',
                        completion: 'Values',
                        index: 44,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'Values',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'Values',
                  sortText: '00000000000000000044',
                  textEdit: {
                    newText: 'Values',
                    range: {
                      end: {
                        character: 18,
                        line: 27,
                      },
                      start: {
                        character: 18,
                        line: 27,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'jsx attribute',
                      {
                        ac_type: 'Acjsx',
                        completion: 'aaaa',
                        index: 0,
                        session_requests: 1,
                        token: 'aAUTO332',
                        typed_length: 1,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'aaaa',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: 'aaaa',
                    range: {
                      end: {
                        character: 4,
                        line: 9,
                      },
                      start: {
                        character: 3,
                        line: 9,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'jsx attribute',
                      {
                        ac_type: 'Acjsx',
                        completion: 'aaab',
                        index: 1,
                        session_requests: 1,
                        token: 'aAUTO332',
                        typed_length: 1,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'aaab',
                  sortText: '00000000000000000001',
                  textEdit: {
                    newText: 'aaab',
                    range: {
                      end: {
                        character: 4,
                        line: 9,
                      },
                      start: {
                        character: 3,
                        line: 9,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'bracket syntax member',
                      {
                        ac_type: 'Acmem',
                        completion: '"a"',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: '"a"',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: '"a"',
                    range: {
                      end: {
                        character: 12,
                        line: 12,
                      },
                      start: {
                        character: 12,
                        line: 12,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'bracket syntax member',
                      {
                        ac_type: 'Acmem',
                        completion: '"b"',
                        index: 1,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'string',
                  insertTextFormat: 1,
                  kind: 6,
                  label: '"b"',
                  sortText: '00000000000000000001',
                  textEdit: {
                    newText: '"b"',
                    range: {
                      end: {
                        character: 12,
                        line: 12,
                      },
                      start: {
                        character: 12,
                        line: 12,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acmem',
                        completion: 'a',
                        index: 2,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'empty',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'a',
                  sortText: '00000000000000000002',
                  textEdit: {
                    newText: 'a',
                    range: {
                      end: {
                        character: 12,
                        line: 12,
                      },
                      start: {
                        character: 12,
                        line: 12,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'local value identifier',
                      {
                        ac_type: 'Acmem',
                        completion: 'o',
                        index: 3,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '{|a: number, b: string|}',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'o',
                  sortText: '00000000000000000003',
                  textEdit: {
                    newText: 'o',
                    range: {
                      end: {
                        character: 12,
                        line: 12,
                      },
                      start: {
                        character: 12,
                        line: 12,
                      },
                    },
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
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'bracket syntax member',
                      {
                        ac_type: 'Acmem',
                        completion: '"bar"',
                        index: 0,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'string',
                  insertTextFormat: 1,
                  kind: 6,
                  label: '"bar"',
                  sortText: '00000000000000000000',
                  textEdit: {
                    newText: '"bar"',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'bracket syntax member',
                      {
                        ac_type: 'Acmem',
                        completion: '"foo"',
                        index: 1,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'boolean',
                  insertTextFormat: 1,
                  kind: 6,
                  label: '"foo"',
                  sortText: '00000000000000000001',
                  textEdit: {
                    newText: '"foo"',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        ac_type: 'Acmem',
                        completion: 'B',
                        index: 2,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'type B = T[any]',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'B',
                  sortText: '00000000000000000002',
                  textEdit: {
                    newText: 'B',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'unqualified type: local type identifier',
                      {
                        ac_type: 'Acmem',
                        completion: 'T',
                        index: 3,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'type T = {|bar: string, foo: boolean|}',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'T',
                  sortText: '00000000000000000003',
                  textEdit: {
                    newText: 'T',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type operators',
                      {
                        ac_type: 'Acmem',
                        completion: 'component',
                        index: 4,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'component',
                  documentation: {
                    kind: 'markdown',
                    value:
                      '[component type](https://flow.org/en/docs/react/component-types/)',
                  },
                  insertTextFormat: 2,
                  kind: 14,
                  label: 'component',
                  sortText: '00000000000000000004',
                  textEdit: {
                    newText: 'component($1)',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type operators',
                      {
                        ac_type: 'Acmem',
                        completion: 'hook',
                        index: 5,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'hook',
                  documentation: {
                    kind: 'markdown',
                    value:
                      '[hook type](https://flow.org/en/docs/react/hook-syntax/#hook-type-annotations)',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'hook',
                  sortText: '00000000000000000005',
                  textEdit: {
                    newText: 'hook ',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type operators',
                      {
                        ac_type: 'Acmem',
                        completion: 'renders',
                        index: 6,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'renders',
                  documentation: {
                    kind: 'markdown',
                    value:
                      '`renders A` means that it will eventually render exactly one React element `A`. See https://flow.org/en/docs/react/render-types/ for more details.',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'renders',
                  sortText: '00000000000000000006',
                  textEdit: {
                    newText: 'renders ',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type operators',
                      {
                        ac_type: 'Acmem',
                        completion: 'renders*',
                        index: 7,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'renders*',
                  documentation: {
                    kind: 'markdown',
                    value:
                      '`renders* A` means that it will eventually render any amount of `A`. See https://flow.org/en/docs/react/render-types/ for more details.',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'renders*',
                  sortText: '00000000000000000007',
                  textEdit: {
                    newText: 'renders* ',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type operators',
                      {
                        ac_type: 'Acmem',
                        completion: 'renders?',
                        index: 8,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'renders?',
                  documentation: {
                    kind: 'markdown',
                    value:
                      '`renders? A` means that it will eventually render zero or one React element `A`. See https://flow.org/en/docs/react/render-types/ for more details.',
                  },
                  insertTextFormat: 1,
                  kind: 14,
                  label: 'renders?',
                  sortText: '00000000000000000008',
                  textEdit: {
                    newText: 'renders? ',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'any',
                        index: 9,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'any',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'any',
                  sortText: '00000000000000000009',
                  textEdit: {
                    newText: 'any',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'bigint',
                        index: 10,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'bigint',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'bigint',
                  sortText: '00000000000000000010',
                  textEdit: {
                    newText: 'bigint',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'boolean',
                        index: 11,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'boolean',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'boolean',
                  sortText: '00000000000000000011',
                  textEdit: {
                    newText: 'boolean',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'empty',
                        index: 12,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'empty',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'empty',
                  sortText: '00000000000000000012',
                  textEdit: {
                    newText: 'empty',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'false',
                        index: 13,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'false',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'false',
                  sortText: '00000000000000000013',
                  textEdit: {
                    newText: 'false',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'mixed',
                        index: 14,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'mixed',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'mixed',
                  sortText: '00000000000000000014',
                  textEdit: {
                    newText: 'mixed',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'null',
                        index: 15,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'null',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'null',
                  sortText: '00000000000000000015',
                  textEdit: {
                    newText: 'null',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'number',
                        index: 16,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'number',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'number',
                  sortText: '00000000000000000016',
                  textEdit: {
                    newText: 'number',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'string',
                        index: 17,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'string',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'string',
                  sortText: '00000000000000000017',
                  textEdit: {
                    newText: 'string',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'symbol',
                        index: 18,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'symbol',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'symbol',
                  sortText: '00000000000000000018',
                  textEdit: {
                    newText: 'symbol',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'true',
                        index: 19,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'true',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'true',
                  sortText: '00000000000000000019',
                  textEdit: {
                    newText: 'true',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'unknown',
                        index: 20,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'unknown',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'unknown',
                  sortText: '00000000000000000020',
                  textEdit: {
                    newText: 'unknown',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'void',
                        index: 21,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'void',
                  insertTextFormat: 1,
                  kind: 6,
                  label: 'void',
                  sortText: '00000000000000000021',
                  textEdit: {
                    newText: 'void',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: '$Exact',
                        index: 22,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$Exact',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$Exact',
                  sortText: '00000000000000000022',
                  textEdit: {
                    newText: '$Exact',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: '$Exports',
                        index: 23,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$Exports',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$Exports',
                  sortText: '00000000000000000023',
                  textEdit: {
                    newText: '$Exports',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: '$KeyMirror',
                        index: 24,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$KeyMirror',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$KeyMirror',
                  sortText: '00000000000000000024',
                  textEdit: {
                    newText: '$KeyMirror',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: '$Keys',
                        index: 25,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$Keys',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$Keys',
                  sortText: '00000000000000000025',
                  textEdit: {
                    newText: '$Keys',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: '$NonMaybeType',
                        index: 26,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$NonMaybeType',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$NonMaybeType',
                  sortText: '00000000000000000026',
                  textEdit: {
                    newText: '$NonMaybeType',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: '$ReadOnly',
                        index: 27,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$ReadOnly',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$ReadOnly',
                  sortText: '00000000000000000027',
                  textEdit: {
                    newText: '$ReadOnly',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: '$Values',
                        index: 28,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: '$Values',
                  insertTextFormat: 1,
                  kind: 3,
                  label: '$Values',
                  sortText: '00000000000000000028',
                  textEdit: {
                    newText: '$Values',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'Class',
                        index: 29,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'Class',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'Class',
                  sortText: '00000000000000000029',
                  textEdit: {
                    newText: 'Class',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'keyof',
                        index: 30,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'keyof',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'keyof',
                  sortText: '00000000000000000030',
                  textEdit: {
                    newText: 'keyof',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'NonNullable',
                        index: 31,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'NonNullable',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'NonNullable',
                  sortText: '00000000000000000031',
                  textEdit: {
                    newText: 'NonNullable',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'Partial',
                        index: 32,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'Partial',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'Partial',
                  sortText: '00000000000000000032',
                  textEdit: {
                    newText: 'Partial',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'Readonly',
                        index: 33,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'Readonly',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'Readonly',
                  sortText: '00000000000000000033',
                  textEdit: {
                    newText: 'Readonly',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'Required',
                        index: 34,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'Required',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'Required',
                  sortText: '00000000000000000034',
                  textEdit: {
                    newText: 'Required',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'StringPrefix',
                        index: 35,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'StringPrefix',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'StringPrefix',
                  sortText: '00000000000000000035',
                  textEdit: {
                    newText: 'StringPrefix',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'StringSuffix',
                        index: 36,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'StringSuffix',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'StringSuffix',
                  sortText: '00000000000000000036',
                  textEdit: {
                    newText: 'StringSuffix',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
                  },
                },
                {
                  command: {
                    arguments: [
                      'textDocument/completion',
                      'builtin type',
                      {
                        ac_type: 'Acmem',
                        completion: 'Values',
                        index: 37,
                        session_requests: 1,
                        token: 'AUTO332',
                        typed_length: 0,
                      },
                    ],
                    command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                    title: '',
                  },
                  detail: 'Values',
                  insertTextFormat: 1,
                  kind: 3,
                  label: 'Values',
                  sortText: '00000000000000000037',
                  textEdit: {
                    newText: 'Values',
                    range: {
                      end: {
                        character: 11,
                        line: 14,
                      },
                      start: {
                        character: 11,
                        line: 14,
                      },
                    },
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
