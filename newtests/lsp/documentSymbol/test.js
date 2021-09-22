/**
 * @flow
 * @format
 */

import type Suite from 'flow-dev-tools/src/test/Suite';
import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default (suite(
  ({
    lspStartAndConnect,
    lspInitializeParams,
    lspRequestAndWaitUntilResponse,
    addFile,
  }) => [
    test('textDocument/documentSymbol with hierarchical support', [
      addFile('stuff.js'),
      lspStartAndConnect(6000, {
        ...lspInitializeParams,
        capabilities: {
          ...lspInitializeParams.capabilities,
          textDocument: {
            ...lspInitializeParams.capabilities.textDocument,
            documentSymbol: {
              hierarchicalDocumentSymbolSupport: true,
            },
          },
        },
      }),
      lspRequestAndWaitUntilResponse('textDocument/documentSymbol', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js'},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/documentSymbol',
            result: [
              {
                name: 'x',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 2,
                      character: 6,
                    },
                    end: {
                      line: 7,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'Y',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 9,
                      character: 0,
                    },
                    end: {
                      line: 19,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 6,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 10,
                      character: 2,
                    },
                    end: {
                      line: 10,
                      character: 11,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'foo',
                kind: 6,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 11,
                      character: 2,
                    },
                    end: {
                      line: 11,
                      character: 10,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'bar',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 12,
                      character: 2,
                    },
                    end: {
                      line: 12,
                      character: 12,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'baz',
                kind: 8,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 13,
                      character: 2,
                    },
                    end: {
                      line: 13,
                      character: 15,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'abc',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 14,
                      character: 2,
                    },
                    end: {
                      line: 14,
                      character: 14,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'abc',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 15,
                      character: 2,
                    },
                    end: {
                      line: 15,
                      character: 23,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'Z',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 16,
                      character: 2,
                    },
                    end: {
                      line: 18,
                      character: 3,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'z',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 21,
                      character: 6,
                    },
                    end: {
                      line: 23,
                      character: 2,
                    },
                  },
                },
              },
              {
                name: 'Z1',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 25,
                      character: 6,
                    },
                    end: {
                      line: 27,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'Z2',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 29,
                      character: 6,
                    },
                    end: {
                      line: 31,
                      character: 2,
                    },
                  },
                },
              },
              {
                name: 'Z3',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 33,
                      character: 6,
                    },
                    end: {
                      line: 35,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'z4',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 38,
                      character: 6,
                    },
                    end: {
                      line: 40,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'Z5',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 42,
                      character: 0,
                    },
                    end: {
                      line: 44,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 43,
                      character: 2,
                    },
                    end: {
                      line: 43,
                      character: 13,
                    },
                  },
                },
                containerName: 'Z5',
              },
              {
                name: 'Z6',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 46,
                      character: 15,
                    },
                    end: {
                      line: 48,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 47,
                      character: 2,
                    },
                    end: {
                      line: 47,
                      character: 13,
                    },
                  },
                },
                containerName: 'Z6',
              },
              {
                name: 'Z7',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 50,
                      character: 7,
                    },
                    end: {
                      line: 52,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 6,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 51,
                      character: 2,
                    },
                    end: {
                      line: 51,
                      character: 10,
                    },
                  },
                },
                containerName: 'Z7',
              },
              {
                name: 'f1',
                kind: 12,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 54,
                      character: 0,
                    },
                    end: {
                      line: 56,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'f2',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 58,
                      character: 6,
                    },
                    end: {
                      line: 60,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'f3',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 62,
                      character: 6,
                    },
                    end: {
                      line: 64,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'f4',
                kind: 12,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 66,
                      character: 0,
                    },
                    end: {
                      line: 66,
                      character: 28,
                    },
                  },
                },
              },
              {
                name: 'f5',
                kind: 12,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 68,
                      character: 15,
                    },
                    end: {
                      line: 68,
                      character: 35,
                    },
                  },
                },
              },
              {
                name: 'f6',
                kind: 12,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 70,
                      character: 7,
                    },
                    end: {
                      line: 72,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'T1',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 78,
                      character: 0,
                    },
                    end: {
                      line: 83,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 79,
                      character: 2,
                    },
                    end: {
                      line: 79,
                      character: 13,
                    },
                  },
                },
                containerName: 'T1',
              },
              {
                name: 'bar',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 80,
                      character: 2,
                    },
                    end: {
                      line: 80,
                      character: 22,
                    },
                  },
                },
                containerName: 'T1',
              },
              {
                name: 'key',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 81,
                      character: 2,
                    },
                    end: {
                      line: 81,
                      character: 23,
                    },
                  },
                },
                containerName: 'T1',
              },
              {
                name: 'call',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 82,
                      character: 2,
                    },
                    end: {
                      line: 82,
                      character: 16,
                    },
                  },
                },
                containerName: 'T1',
              },
              {
                name: 'I1',
                kind: 11,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 85,
                      character: 0,
                    },
                    end: {
                      line: 89,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 86,
                      character: 2,
                    },
                    end: {
                      line: 86,
                      character: 13,
                    },
                  },
                },
                containerName: 'I1',
              },
              {
                name: 'bar',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 87,
                      character: 2,
                    },
                    end: {
                      line: 87,
                      character: 22,
                    },
                  },
                },
                containerName: 'I1',
              },
              {
                name: 'key',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 88,
                      character: 2,
                    },
                    end: {
                      line: 88,
                      character: 23,
                    },
                  },
                },
                containerName: 'I1',
              },
              {
                name: 'I3',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 95,
                      character: 0,
                    },
                    end: {
                      line: 97,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 96,
                      character: 2,
                    },
                    end: {
                      line: 96,
                      character: 13,
                    },
                  },
                },
                containerName: 'I3',
              },
              {
                name: 'I4',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 99,
                      character: 0,
                    },
                    end: {
                      line: 101,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'I5',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 103,
                      character: 0,
                    },
                    end: {
                      line: 105,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'decl1',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 107,
                      character: 0,
                    },
                    end: {
                      line: 107,
                      character: 35,
                    },
                  },
                },
              },
              {
                name: 'decl2',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 109,
                      character: 15,
                    },
                    end: {
                      line: 109,
                      character: 42,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 111,
                      character: 25,
                    },
                    end: {
                      line: 111,
                      character: 36,
                    },
                  },
                },
              },
              {
                name: 'M1',
                kind: 2,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 117,
                      character: 0,
                    },
                    end: {
                      line: 119,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'C',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 118,
                      character: 2,
                    },
                    end: {
                      line: 118,
                      character: 20,
                    },
                  },
                },
                containerName: 'M1',
              },
              {
                name: 'loops',
                kind: 12,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 129,
                      character: 0,
                    },
                    end: {
                      line: 133,
                      character: 1,
                    },
                  },
                },
              },
            ],
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('textDocument/documentSymbol without hierarchical support', [
      addFile('stuff.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/documentSymbol', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js'},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/documentSymbol',
            result: [
              {
                name: 'x',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 2,
                      character: 6,
                    },
                    end: {
                      line: 7,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'Y',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 9,
                      character: 0,
                    },
                    end: {
                      line: 19,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 6,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 10,
                      character: 2,
                    },
                    end: {
                      line: 10,
                      character: 11,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'foo',
                kind: 6,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 11,
                      character: 2,
                    },
                    end: {
                      line: 11,
                      character: 10,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'bar',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 12,
                      character: 2,
                    },
                    end: {
                      line: 12,
                      character: 12,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'baz',
                kind: 8,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 13,
                      character: 2,
                    },
                    end: {
                      line: 13,
                      character: 15,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'abc',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 14,
                      character: 2,
                    },
                    end: {
                      line: 14,
                      character: 14,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'abc',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 15,
                      character: 2,
                    },
                    end: {
                      line: 15,
                      character: 23,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'Z',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 16,
                      character: 2,
                    },
                    end: {
                      line: 18,
                      character: 3,
                    },
                  },
                },
                containerName: 'Y',
              },
              {
                name: 'z',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 21,
                      character: 6,
                    },
                    end: {
                      line: 23,
                      character: 2,
                    },
                  },
                },
              },
              {
                name: 'Z1',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 25,
                      character: 6,
                    },
                    end: {
                      line: 27,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'Z2',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 29,
                      character: 6,
                    },
                    end: {
                      line: 31,
                      character: 2,
                    },
                  },
                },
              },
              {
                name: 'Z3',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 33,
                      character: 6,
                    },
                    end: {
                      line: 35,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'z4',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 38,
                      character: 6,
                    },
                    end: {
                      line: 40,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'Z5',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 42,
                      character: 0,
                    },
                    end: {
                      line: 44,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 43,
                      character: 2,
                    },
                    end: {
                      line: 43,
                      character: 13,
                    },
                  },
                },
                containerName: 'Z5',
              },
              {
                name: 'Z6',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 46,
                      character: 15,
                    },
                    end: {
                      line: 48,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 47,
                      character: 2,
                    },
                    end: {
                      line: 47,
                      character: 13,
                    },
                  },
                },
                containerName: 'Z6',
              },
              {
                name: 'Z7',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 50,
                      character: 7,
                    },
                    end: {
                      line: 52,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 6,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 51,
                      character: 2,
                    },
                    end: {
                      line: 51,
                      character: 10,
                    },
                  },
                },
                containerName: 'Z7',
              },
              {
                name: 'f1',
                kind: 12,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 54,
                      character: 0,
                    },
                    end: {
                      line: 56,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'f2',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 58,
                      character: 6,
                    },
                    end: {
                      line: 60,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'f3',
                kind: 14,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 62,
                      character: 6,
                    },
                    end: {
                      line: 64,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'f4',
                kind: 12,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 66,
                      character: 0,
                    },
                    end: {
                      line: 66,
                      character: 28,
                    },
                  },
                },
              },
              {
                name: 'f5',
                kind: 12,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 68,
                      character: 15,
                    },
                    end: {
                      line: 68,
                      character: 35,
                    },
                  },
                },
              },
              {
                name: 'f6',
                kind: 12,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 70,
                      character: 7,
                    },
                    end: {
                      line: 72,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'T1',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 78,
                      character: 0,
                    },
                    end: {
                      line: 83,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 79,
                      character: 2,
                    },
                    end: {
                      line: 79,
                      character: 13,
                    },
                  },
                },
                containerName: 'T1',
              },
              {
                name: 'bar',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 80,
                      character: 2,
                    },
                    end: {
                      line: 80,
                      character: 22,
                    },
                  },
                },
                containerName: 'T1',
              },
              {
                name: 'key',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 81,
                      character: 2,
                    },
                    end: {
                      line: 81,
                      character: 23,
                    },
                  },
                },
                containerName: 'T1',
              },
              {
                name: 'call',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 82,
                      character: 2,
                    },
                    end: {
                      line: 82,
                      character: 16,
                    },
                  },
                },
                containerName: 'T1',
              },
              {
                name: 'I1',
                kind: 11,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 85,
                      character: 0,
                    },
                    end: {
                      line: 89,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 86,
                      character: 2,
                    },
                    end: {
                      line: 86,
                      character: 13,
                    },
                  },
                },
                containerName: 'I1',
              },
              {
                name: 'bar',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 87,
                      character: 2,
                    },
                    end: {
                      line: 87,
                      character: 22,
                    },
                  },
                },
                containerName: 'I1',
              },
              {
                name: 'key',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 88,
                      character: 2,
                    },
                    end: {
                      line: 88,
                      character: 23,
                    },
                  },
                },
                containerName: 'I1',
              },
              {
                name: 'I3',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 95,
                      character: 0,
                    },
                    end: {
                      line: 97,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 96,
                      character: 2,
                    },
                    end: {
                      line: 96,
                      character: 13,
                    },
                  },
                },
                containerName: 'I3',
              },
              {
                name: 'I4',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 99,
                      character: 0,
                    },
                    end: {
                      line: 101,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'I5',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 103,
                      character: 0,
                    },
                    end: {
                      line: 105,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'decl1',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 107,
                      character: 0,
                    },
                    end: {
                      line: 107,
                      character: 35,
                    },
                  },
                },
              },
              {
                name: 'decl2',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 109,
                      character: 15,
                    },
                    end: {
                      line: 109,
                      character: 42,
                    },
                  },
                },
              },
              {
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 111,
                      character: 25,
                    },
                    end: {
                      line: 111,
                      character: 36,
                    },
                  },
                },
              },
              {
                name: 'M1',
                kind: 2,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 117,
                      character: 0,
                    },
                    end: {
                      line: 119,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'C',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 118,
                      character: 2,
                    },
                    end: {
                      line: 118,
                      character: 20,
                    },
                  },
                },
                containerName: 'M1',
              },
              {
                name: 'loops',
                kind: 12,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 129,
                      character: 0,
                    },
                    end: {
                      line: 133,
                      character: 1,
                    },
                  },
                },
              },
            ],
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
