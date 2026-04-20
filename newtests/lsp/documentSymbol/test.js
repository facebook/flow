/**
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {suite, test} = require('../../Tester');

module.exports = (suite(
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
                children: [
                  {
                    children: [
                      {
                        kind: 7,
                        name: 'nested',
                        range: {
                          end: {
                            character: 22,
                            line: 6,
                          },
                          start: {
                            character: 9,
                            line: 6,
                          },
                        },
                        selectionRange: {
                          end: {
                            character: 15,
                            line: 6,
                          },
                          start: {
                            character: 9,
                            line: 6,
                          },
                        },
                      },
                    ],
                    kind: 7,
                    name: 'obj',
                    range: {
                      end: {
                        character: 24,
                        line: 6,
                      },
                      start: {
                        character: 2,
                        line: 6,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 6,
                      },
                      start: {
                        character: 2,
                        line: 6,
                      },
                    },
                  },
                  {
                    kind: 6,
                    name: 'bar',
                    range: {
                      end: {
                        character: 20,
                        line: 5,
                      },
                      start: {
                        character: 2,
                        line: 5,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 5,
                      },
                      start: {
                        character: 2,
                        line: 5,
                      },
                    },
                  },
                  {
                    kind: 7,
                    name: 'foo',
                    range: {
                      end: {
                        character: 10,
                        line: 4,
                      },
                      start: {
                        character: 2,
                        line: 4,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 4,
                      },
                      start: {
                        character: 2,
                        line: 4,
                      },
                    },
                  },
                  {
                    kind: 5,
                    name: 'C',
                    range: {
                      end: {
                        character: 13,
                        line: 3,
                      },
                      start: {
                        character: 2,
                        line: 3,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 3,
                        line: 3,
                      },
                      start: {
                        character: 2,
                        line: 3,
                      },
                    },
                  },
                ],
                kind: 13,
                name: 'x',
                range: {
                  end: {
                    character: 1,
                    line: 7,
                  },
                  start: {
                    character: 6,
                    line: 2,
                  },
                },
                selectionRange: {
                  end: {
                    character: 7,
                    line: 2,
                  },
                  start: {
                    character: 6,
                    line: 2,
                  },
                },
              },
              {
                children: [
                  {
                    children: [
                      {
                        kind: 6,
                        name: 'zmeth',
                        range: {
                          end: {
                            character: 14,
                            line: 17,
                          },
                          start: {
                            character: 4,
                            line: 17,
                          },
                        },
                        selectionRange: {
                          end: {
                            character: 9,
                            line: 17,
                          },
                          start: {
                            character: 4,
                            line: 17,
                          },
                        },
                      },
                    ],
                    kind: 5,
                    name: 'Z',
                    range: {
                      end: {
                        character: 3,
                        line: 18,
                      },
                      start: {
                        character: 2,
                        line: 16,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 3,
                        line: 16,
                      },
                      start: {
                        character: 2,
                        line: 16,
                      },
                    },
                  },
                  {
                    kind: 7,
                    name: '(set) abc',
                    range: {
                      end: {
                        character: 23,
                        line: 15,
                      },
                      start: {
                        character: 2,
                        line: 15,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 9,
                        line: 15,
                      },
                      start: {
                        character: 6,
                        line: 15,
                      },
                    },
                  },
                  {
                    kind: 7,
                    name: '(get) abc',
                    range: {
                      end: {
                        character: 14,
                        line: 14,
                      },
                      start: {
                        character: 2,
                        line: 14,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 9,
                        line: 14,
                      },
                      start: {
                        character: 6,
                        line: 14,
                      },
                    },
                  },
                  {
                    kind: 7,
                    name: '#baz',
                    range: {
                      end: {
                        character: 15,
                        line: 13,
                      },
                      start: {
                        character: 2,
                        line: 13,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 6,
                        line: 13,
                      },
                      start: {
                        character: 2,
                        line: 13,
                      },
                    },
                  },
                  {
                    kind: 7,
                    name: 'bar',
                    range: {
                      end: {
                        character: 12,
                        line: 12,
                      },
                      start: {
                        character: 2,
                        line: 12,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 12,
                      },
                      start: {
                        character: 2,
                        line: 12,
                      },
                    },
                  },
                  {
                    kind: 6,
                    name: 'foo',
                    range: {
                      end: {
                        character: 10,
                        line: 11,
                      },
                      start: {
                        character: 2,
                        line: 11,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 11,
                      },
                      start: {
                        character: 2,
                        line: 11,
                      },
                    },
                  },
                  {
                    kind: 6,
                    name: '#foo',
                    range: {
                      end: {
                        character: 11,
                        line: 10,
                      },
                      start: {
                        character: 2,
                        line: 10,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 6,
                        line: 10,
                      },
                      start: {
                        character: 2,
                        line: 10,
                      },
                    },
                  },
                ],
                kind: 5,
                name: 'Y',
                range: {
                  end: {
                    character: 1,
                    line: 19,
                  },
                  start: {
                    character: 0,
                    line: 9,
                  },
                },
                selectionRange: {
                  end: {
                    character: 7,
                    line: 9,
                  },
                  start: {
                    character: 6,
                    line: 9,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 6,
                    name: 'foo',
                    range: {
                      end: {
                        character: 10,
                        line: 22,
                      },
                      start: {
                        character: 2,
                        line: 22,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 22,
                      },
                      start: {
                        character: 2,
                        line: 22,
                      },
                    },
                  },
                ],
                kind: 5,
                name: 'z',
                range: {
                  end: {
                    character: 2,
                    line: 23,
                  },
                  start: {
                    character: 6,
                    line: 21,
                  },
                },
                selectionRange: {
                  end: {
                    character: 7,
                    line: 21,
                  },
                  start: {
                    character: 6,
                    line: 21,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 6,
                    name: 'foo',
                    range: {
                      end: {
                        character: 10,
                        line: 26,
                      },
                      start: {
                        character: 2,
                        line: 26,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 26,
                      },
                      start: {
                        character: 2,
                        line: 26,
                      },
                    },
                  },
                ],
                kind: 5,
                name: 'Z1',
                range: {
                  end: {
                    character: 1,
                    line: 27,
                  },
                  start: {
                    character: 6,
                    line: 25,
                  },
                },
                selectionRange: {
                  end: {
                    character: 8,
                    line: 25,
                  },
                  start: {
                    character: 6,
                    line: 25,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 6,
                    name: 'foo',
                    range: {
                      end: {
                        character: 10,
                        line: 30,
                      },
                      start: {
                        character: 2,
                        line: 30,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 30,
                      },
                      start: {
                        character: 2,
                        line: 30,
                      },
                    },
                  },
                ],
                kind: 5,
                name: 'Z2',
                range: {
                  end: {
                    character: 2,
                    line: 31,
                  },
                  start: {
                    character: 6,
                    line: 29,
                  },
                },
                selectionRange: {
                  end: {
                    character: 8,
                    line: 29,
                  },
                  start: {
                    character: 6,
                    line: 29,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 6,
                    name: 'foo',
                    range: {
                      end: {
                        character: 10,
                        line: 34,
                      },
                      start: {
                        character: 2,
                        line: 34,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 34,
                      },
                      start: {
                        character: 2,
                        line: 34,
                      },
                    },
                  },
                ],
                kind: 5,
                name: 'Z3',
                range: {
                  end: {
                    character: 1,
                    line: 35,
                  },
                  start: {
                    character: 6,
                    line: 33,
                  },
                },
                selectionRange: {
                  end: {
                    character: 8,
                    line: 33,
                  },
                  start: {
                    character: 6,
                    line: 33,
                  },
                },
              },
              {
                children: [
                  {
                    children: [
                      {
                        kind: 6,
                        name: 'foo',
                        range: {
                          end: {
                            character: 10,
                            line: 39,
                          },
                          start: {
                            character: 2,
                            line: 39,
                          },
                        },
                        selectionRange: {
                          end: {
                            character: 5,
                            line: 39,
                          },
                          start: {
                            character: 2,
                            line: 39,
                          },
                        },
                      },
                    ],
                    kind: 5,
                    name: 'Z4',
                    range: {
                      end: {
                        character: 1,
                        line: 40,
                      },
                      start: {
                        character: 11,
                        line: 38,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 19,
                        line: 38,
                      },
                      start: {
                        character: 17,
                        line: 38,
                      },
                    },
                  },
                ],
                kind: 13,
                name: 'z4',
                range: {
                  end: {
                    character: 1,
                    line: 40,
                  },
                  start: {
                    character: 6,
                    line: 38,
                  },
                },
                selectionRange: {
                  end: {
                    character: 8,
                    line: 38,
                  },
                  start: {
                    character: 6,
                    line: 38,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 6,
                    name: 'foo',
                    range: {
                      end: {
                        character: 13,
                        line: 43,
                      },
                      start: {
                        character: 2,
                        line: 43,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 43,
                      },
                      start: {
                        character: 2,
                        line: 43,
                      },
                    },
                  },
                ],
                kind: 5,
                name: 'Z5',
                range: {
                  end: {
                    character: 1,
                    line: 44,
                  },
                  start: {
                    character: 0,
                    line: 42,
                  },
                },
                selectionRange: {
                  end: {
                    character: 16,
                    line: 42,
                  },
                  start: {
                    character: 14,
                    line: 42,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 6,
                    name: 'foo',
                    range: {
                      end: {
                        character: 13,
                        line: 47,
                      },
                      start: {
                        character: 2,
                        line: 47,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 47,
                      },
                      start: {
                        character: 2,
                        line: 47,
                      },
                    },
                  },
                ],
                kind: 5,
                name: 'Z6',
                range: {
                  end: {
                    character: 1,
                    line: 48,
                  },
                  start: {
                    character: 15,
                    line: 46,
                  },
                },
                selectionRange: {
                  end: {
                    character: 23,
                    line: 46,
                  },
                  start: {
                    character: 21,
                    line: 46,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 6,
                    name: 'foo',
                    range: {
                      end: {
                        character: 10,
                        line: 51,
                      },
                      start: {
                        character: 2,
                        line: 51,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 51,
                      },
                      start: {
                        character: 2,
                        line: 51,
                      },
                    },
                  },
                ],
                kind: 5,
                name: 'Z7',
                range: {
                  end: {
                    character: 1,
                    line: 52,
                  },
                  start: {
                    character: 7,
                    line: 50,
                  },
                },
                selectionRange: {
                  end: {
                    character: 15,
                    line: 50,
                  },
                  start: {
                    character: 13,
                    line: 50,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 13,
                    name: 'f1_x',
                    range: {
                      end: {
                        character: 18,
                        line: 55,
                      },
                      start: {
                        character: 8,
                        line: 55,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 12,
                        line: 55,
                      },
                      start: {
                        character: 8,
                        line: 55,
                      },
                    },
                  },
                ],
                kind: 12,
                name: 'f1',
                range: {
                  end: {
                    character: 1,
                    line: 56,
                  },
                  start: {
                    character: 0,
                    line: 54,
                  },
                },
                selectionRange: {
                  end: {
                    character: 11,
                    line: 54,
                  },
                  start: {
                    character: 9,
                    line: 54,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 13,
                    name: 'f2_x',
                    range: {
                      end: {
                        character: 18,
                        line: 59,
                      },
                      start: {
                        character: 8,
                        line: 59,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 12,
                        line: 59,
                      },
                      start: {
                        character: 8,
                        line: 59,
                      },
                    },
                  },
                ],
                kind: 6,
                name: 'f2',
                range: {
                  end: {
                    character: 1,
                    line: 60,
                  },
                  start: {
                    character: 6,
                    line: 58,
                  },
                },
                selectionRange: {
                  end: {
                    character: 8,
                    line: 58,
                  },
                  start: {
                    character: 6,
                    line: 58,
                  },
                },
              },
              {
                children: [
                  {
                    children: [
                      {
                        kind: 13,
                        name: 'f3_x',
                        range: {
                          end: {
                            character: 18,
                            line: 63,
                          },
                          start: {
                            character: 8,
                            line: 63,
                          },
                        },
                        selectionRange: {
                          end: {
                            character: 12,
                            line: 63,
                          },
                          start: {
                            character: 8,
                            line: 63,
                          },
                        },
                      },
                    ],
                    kind: 12,
                    name: 'f3_expr',
                    range: {
                      end: {
                        character: 1,
                        line: 64,
                      },
                      start: {
                        character: 11,
                        line: 62,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 27,
                        line: 62,
                      },
                      start: {
                        character: 20,
                        line: 62,
                      },
                    },
                  },
                ],
                kind: 13,
                name: 'f3',
                range: {
                  end: {
                    character: 1,
                    line: 64,
                  },
                  start: {
                    character: 6,
                    line: 62,
                  },
                },
                selectionRange: {
                  end: {
                    character: 8,
                    line: 62,
                  },
                  start: {
                    character: 6,
                    line: 62,
                  },
                },
              },
              {
                kind: 12,
                name: 'f4',
                range: {
                  end: {
                    character: 28,
                    line: 66,
                  },
                  start: {
                    character: 0,
                    line: 66,
                  },
                },
                selectionRange: {
                  end: {
                    character: 19,
                    line: 66,
                  },
                  start: {
                    character: 17,
                    line: 66,
                  },
                },
              },
              {
                kind: 12,
                name: 'f5',
                range: {
                  end: {
                    character: 35,
                    line: 68,
                  },
                  start: {
                    character: 15,
                    line: 68,
                  },
                },
                selectionRange: {
                  end: {
                    character: 26,
                    line: 68,
                  },
                  start: {
                    character: 24,
                    line: 68,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 13,
                    name: 'f6_x',
                    range: {
                      end: {
                        character: 18,
                        line: 71,
                      },
                      start: {
                        character: 8,
                        line: 71,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 12,
                        line: 71,
                      },
                      start: {
                        character: 8,
                        line: 71,
                      },
                    },
                  },
                ],
                kind: 12,
                name: 'f6',
                range: {
                  end: {
                    character: 1,
                    line: 72,
                  },
                  start: {
                    character: 7,
                    line: 70,
                  },
                },
                selectionRange: {
                  end: {
                    character: 18,
                    line: 70,
                  },
                  start: {
                    character: 16,
                    line: 70,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 13,
                    name: 'iife_x',
                    range: {
                      end: {
                        character: 20,
                        line: 75,
                      },
                      start: {
                        character: 8,
                        line: 75,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 14,
                        line: 75,
                      },
                      start: {
                        character: 8,
                        line: 75,
                      },
                    },
                  },
                ],
                kind: 12,
                name: '<function>',
                range: {
                  end: {
                    character: 1,
                    line: 76,
                  },
                  start: {
                    character: 1,
                    line: 74,
                  },
                },
                selectionRange: {
                  end: {
                    character: 1,
                    line: 76,
                  },
                  start: {
                    character: 1,
                    line: 74,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 7,
                    name: '[[call]]',
                    range: {
                      end: {
                        character: 16,
                        line: 82,
                      },
                      start: {
                        character: 2,
                        line: 82,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 8,
                        line: 82,
                      },
                      start: {
                        character: 4,
                        line: 82,
                      },
                    },
                  },
                  {
                    kind: 7,
                    name: '[key]',
                    range: {
                      end: {
                        character: 23,
                        line: 81,
                      },
                      start: {
                        character: 2,
                        line: 81,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 6,
                        line: 81,
                      },
                      start: {
                        character: 3,
                        line: 81,
                      },
                    },
                  },
                  {
                    children: [
                      {
                        kind: 7,
                        name: 'baz',
                        range: {
                          end: {
                            character: 20,
                            line: 80,
                          },
                          start: {
                            character: 9,
                            line: 80,
                          },
                        },
                        selectionRange: {
                          end: {
                            character: 12,
                            line: 80,
                          },
                          start: {
                            character: 9,
                            line: 80,
                          },
                        },
                      },
                    ],
                    kind: 7,
                    name: 'bar',
                    range: {
                      end: {
                        character: 22,
                        line: 80,
                      },
                      start: {
                        character: 2,
                        line: 80,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 80,
                      },
                      start: {
                        character: 2,
                        line: 80,
                      },
                    },
                  },
                  {
                    kind: 6,
                    name: 'foo',
                    range: {
                      end: {
                        character: 13,
                        line: 79,
                      },
                      start: {
                        character: 2,
                        line: 79,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 79,
                      },
                      start: {
                        character: 2,
                        line: 79,
                      },
                    },
                  },
                ],
                kind: 13,
                name: 'T1',
                range: {
                  end: {
                    character: 1,
                    line: 83,
                  },
                  start: {
                    character: 0,
                    line: 78,
                  },
                },
                selectionRange: {
                  end: {
                    character: 7,
                    line: 78,
                  },
                  start: {
                    character: 5,
                    line: 78,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 7,
                    name: '[key]',
                    range: {
                      end: {
                        character: 23,
                        line: 88,
                      },
                      start: {
                        character: 2,
                        line: 88,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 6,
                        line: 88,
                      },
                      start: {
                        character: 3,
                        line: 88,
                      },
                    },
                  },
                  {
                    children: [
                      {
                        kind: 7,
                        name: 'baz',
                        range: {
                          end: {
                            character: 20,
                            line: 87,
                          },
                          start: {
                            character: 9,
                            line: 87,
                          },
                        },
                        selectionRange: {
                          end: {
                            character: 12,
                            line: 87,
                          },
                          start: {
                            character: 9,
                            line: 87,
                          },
                        },
                      },
                    ],
                    kind: 7,
                    name: 'bar',
                    range: {
                      end: {
                        character: 22,
                        line: 87,
                      },
                      start: {
                        character: 2,
                        line: 87,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 87,
                      },
                      start: {
                        character: 2,
                        line: 87,
                      },
                    },
                  },
                  {
                    kind: 6,
                    name: 'foo',
                    range: {
                      end: {
                        character: 13,
                        line: 86,
                      },
                      start: {
                        character: 2,
                        line: 86,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 86,
                      },
                      start: {
                        character: 2,
                        line: 86,
                      },
                    },
                  },
                ],
                kind: 11,
                name: 'I1',
                range: {
                  end: {
                    character: 1,
                    line: 89,
                  },
                  start: {
                    character: 0,
                    line: 85,
                  },
                },
                selectionRange: {
                  end: {
                    character: 12,
                    line: 85,
                  },
                  start: {
                    character: 10,
                    line: 85,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 6,
                    name: 'foo',
                    range: {
                      end: {
                        character: 13,
                        line: 92,
                      },
                      start: {
                        character: 2,
                        line: 92,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 92,
                      },
                      start: {
                        character: 2,
                        line: 92,
                      },
                    },
                  },
                ],
                kind: 11,
                name: 'I2',
                range: {
                  end: {
                    character: 1,
                    line: 93,
                  },
                  start: {
                    character: 0,
                    line: 91,
                  },
                },
                selectionRange: {
                  end: {
                    character: 20,
                    line: 91,
                  },
                  start: {
                    character: 18,
                    line: 91,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 6,
                    name: 'foo',
                    range: {
                      end: {
                        character: 13,
                        line: 96,
                      },
                      start: {
                        character: 2,
                        line: 96,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 96,
                      },
                      start: {
                        character: 2,
                        line: 96,
                      },
                    },
                  },
                ],
                kind: 13,
                name: 'I3',
                range: {
                  end: {
                    character: 1,
                    line: 97,
                  },
                  start: {
                    character: 0,
                    line: 95,
                  },
                },
                selectionRange: {
                  end: {
                    character: 7,
                    line: 95,
                  },
                  start: {
                    character: 5,
                    line: 95,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 7,
                    name: 'foo',
                    range: {
                      end: {
                        character: 13,
                        line: 100,
                      },
                      start: {
                        character: 2,
                        line: 100,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 100,
                      },
                      start: {
                        character: 2,
                        line: 100,
                      },
                    },
                  },
                ],
                kind: 13,
                name: 'I4',
                range: {
                  end: {
                    character: 1,
                    line: 101,
                  },
                  start: {
                    character: 0,
                    line: 99,
                  },
                },
                selectionRange: {
                  end: {
                    character: 14,
                    line: 99,
                  },
                  start: {
                    character: 12,
                    line: 99,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 7,
                    name: 'foo',
                    range: {
                      end: {
                        character: 13,
                        line: 104,
                      },
                      start: {
                        character: 2,
                        line: 104,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 104,
                      },
                      start: {
                        character: 2,
                        line: 104,
                      },
                    },
                  },
                ],
                kind: 13,
                name: 'I5',
                range: {
                  end: {
                    character: 1,
                    line: 105,
                  },
                  start: {
                    character: 0,
                    line: 103,
                  },
                },
                selectionRange: {
                  end: {
                    character: 22,
                    line: 103,
                  },
                  start: {
                    character: 20,
                    line: 103,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 7,
                    name: 'foo',
                    range: {
                      end: {
                        character: 32,
                        line: 107,
                      },
                      start: {
                        character: 21,
                        line: 107,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 24,
                        line: 107,
                      },
                      start: {
                        character: 21,
                        line: 107,
                      },
                    },
                  },
                ],
                kind: 13,
                name: 'decl1',
                range: {
                  end: {
                    character: 35,
                    line: 107,
                  },
                  start: {
                    character: 0,
                    line: 107,
                  },
                },
                selectionRange: {
                  end: {
                    character: 17,
                    line: 107,
                  },
                  start: {
                    character: 12,
                    line: 107,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 7,
                    name: 'foo',
                    range: {
                      end: {
                        character: 39,
                        line: 109,
                      },
                      start: {
                        character: 28,
                        line: 109,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 31,
                        line: 109,
                      },
                      start: {
                        character: 28,
                        line: 109,
                      },
                    },
                  },
                ],
                kind: 13,
                name: 'decl2',
                range: {
                  end: {
                    character: 42,
                    line: 109,
                  },
                  start: {
                    character: 15,
                    line: 109,
                  },
                },
                selectionRange: {
                  end: {
                    character: 24,
                    line: 109,
                  },
                  start: {
                    character: 19,
                    line: 109,
                  },
                },
              },
              {
                kind: 7,
                name: 'foo',
                range: {
                  end: {
                    character: 36,
                    line: 111,
                  },
                  start: {
                    character: 25,
                    line: 111,
                  },
                },
                selectionRange: {
                  end: {
                    character: 28,
                    line: 111,
                  },
                  start: {
                    character: 25,
                    line: 111,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 7,
                    name: 'foo',
                    range: {
                      end: {
                        character: 10,
                        line: 114,
                      },
                      start: {
                        character: 2,
                        line: 114,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 5,
                        line: 114,
                      },
                      start: {
                        character: 2,
                        line: 114,
                      },
                    },
                  },
                ],
                kind: 13,
                name: 'default',
                range: {
                  end: {
                    character: 2,
                    line: 115,
                  },
                  start: {
                    character: 0,
                    line: 113,
                  },
                },
                selectionRange: {
                  end: {
                    character: 14,
                    line: 113,
                  },
                  start: {
                    character: 7,
                    line: 113,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 5,
                    name: 'C',
                    range: {
                      end: {
                        character: 20,
                        line: 118,
                      },
                      start: {
                        character: 2,
                        line: 118,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 17,
                        line: 118,
                      },
                      start: {
                        character: 16,
                        line: 118,
                      },
                    },
                  },
                ],
                kind: 2,
                name: 'M1',
                range: {
                  end: {
                    character: 1,
                    line: 119,
                  },
                  start: {
                    character: 0,
                    line: 117,
                  },
                },
                selectionRange: {
                  end: {
                    character: 17,
                    line: 117,
                  },
                  start: {
                    character: 15,
                    line: 117,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 5,
                    name: 'C',
                    range: {
                      end: {
                        character: 20,
                        line: 122,
                      },
                      start: {
                        character: 2,
                        line: 122,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 17,
                        line: 122,
                      },
                      start: {
                        character: 16,
                        line: 122,
                      },
                    },
                  },
                ],
                kind: 2,
                name: '"M2"',
                range: {
                  end: {
                    character: 1,
                    line: 123,
                  },
                  start: {
                    character: 0,
                    line: 121,
                  },
                },
                selectionRange: {
                  end: {
                    character: 19,
                    line: 121,
                  },
                  start: {
                    character: 15,
                    line: 121,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 22,
                    name: 'X',
                    range: {
                      end: {
                        character: 3,
                        line: 126,
                      },
                      start: {
                        character: 2,
                        line: 126,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 3,
                        line: 126,
                      },
                      start: {
                        character: 2,
                        line: 126,
                      },
                    },
                  },
                ],
                kind: 10,
                name: 'E1',
                range: {
                  end: {
                    character: 1,
                    line: 127,
                  },
                  start: {
                    character: 0,
                    line: 125,
                  },
                },
                selectionRange: {
                  end: {
                    character: 7,
                    line: 125,
                  },
                  start: {
                    character: 5,
                    line: 125,
                  },
                },
              },
              {
                children: [
                  {
                    kind: 13,
                    name: 'k',
                    range: {
                      end: {
                        character: 12,
                        line: 132,
                      },
                      start: {
                        character: 11,
                        line: 132,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 12,
                        line: 132,
                      },
                      start: {
                        character: 11,
                        line: 132,
                      },
                    },
                  },
                  {
                    kind: 7,
                    name: 'x',
                    range: {
                      end: {
                        character: 24,
                        line: 131,
                      },
                      start: {
                        character: 18,
                        line: 131,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 19,
                        line: 131,
                      },
                      start: {
                        character: 18,
                        line: 131,
                      },
                    },
                  },
                  {
                    kind: 13,
                    name: 'j',
                    range: {
                      end: {
                        character: 12,
                        line: 131,
                      },
                      start: {
                        character: 11,
                        line: 131,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 12,
                        line: 131,
                      },
                      start: {
                        character: 11,
                        line: 131,
                      },
                    },
                  },
                  {
                    kind: 13,
                    name: 'i',
                    range: {
                      end: {
                        character: 16,
                        line: 130,
                      },
                      start: {
                        character: 11,
                        line: 130,
                      },
                    },
                    selectionRange: {
                      end: {
                        character: 12,
                        line: 130,
                      },
                      start: {
                        character: 11,
                        line: 130,
                      },
                    },
                  },
                ],
                kind: 12,
                name: 'loops',
                range: {
                  end: {
                    character: 1,
                    line: 133,
                  },
                  start: {
                    character: 0,
                    line: 129,
                  },
                },
                selectionRange: {
                  end: {
                    character: 14,
                    line: 129,
                  },
                  start: {
                    character: 9,
                    line: 129,
                  },
                },
              },
              {
                kind: 12,
                name: 'ComponentFoo',
                range: {
                  end: {
                    character: 27,
                    line: 135,
                  },
                  start: {
                    character: 0,
                    line: 135,
                  },
                },
                selectionRange: {
                  end: {
                    character: 22,
                    line: 135,
                  },
                  start: {
                    character: 10,
                    line: 135,
                  },
                },
              },
              {
                kind: 12,
                name: 'ComponentBar',
                range: {
                  end: {
                    character: 33,
                    line: 136,
                  },
                  start: {
                    character: 0,
                    line: 136,
                  },
                },
                selectionRange: {
                  end: {
                    character: 30,
                    line: 136,
                  },
                  start: {
                    character: 18,
                    line: 136,
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
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 7,
                    },
                    start: {
                      character: 6,
                      line: 2,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'x',
              },
              {
                containerName: 'x',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 24,
                      line: 6,
                    },
                    start: {
                      character: 2,
                      line: 6,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'obj',
              },
              {
                containerName: 'obj',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 22,
                      line: 6,
                    },
                    start: {
                      character: 9,
                      line: 6,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'nested',
              },
              {
                containerName: 'x',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 20,
                      line: 5,
                    },
                    start: {
                      character: 2,
                      line: 5,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'bar',
              },
              {
                containerName: 'x',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 10,
                      line: 4,
                    },
                    start: {
                      character: 2,
                      line: 4,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                containerName: 'x',
                kind: 5,
                location: {
                  range: {
                    end: {
                      character: 13,
                      line: 3,
                    },
                    start: {
                      character: 2,
                      line: 3,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'C',
              },
              {
                kind: 5,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 19,
                    },
                    start: {
                      character: 0,
                      line: 9,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'Y',
              },
              {
                containerName: 'Y',
                kind: 5,
                location: {
                  range: {
                    end: {
                      character: 3,
                      line: 18,
                    },
                    start: {
                      character: 2,
                      line: 16,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'Z',
              },
              {
                containerName: 'Z',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 14,
                      line: 17,
                    },
                    start: {
                      character: 4,
                      line: 17,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'zmeth',
              },
              {
                containerName: 'Y',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 23,
                      line: 15,
                    },
                    start: {
                      character: 2,
                      line: 15,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: '(set) abc',
              },
              {
                containerName: 'Y',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 14,
                      line: 14,
                    },
                    start: {
                      character: 2,
                      line: 14,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: '(get) abc',
              },
              {
                containerName: 'Y',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 15,
                      line: 13,
                    },
                    start: {
                      character: 2,
                      line: 13,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: '#baz',
              },
              {
                containerName: 'Y',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 12,
                      line: 12,
                    },
                    start: {
                      character: 2,
                      line: 12,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'bar',
              },
              {
                containerName: 'Y',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 10,
                      line: 11,
                    },
                    start: {
                      character: 2,
                      line: 11,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                containerName: 'Y',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 11,
                      line: 10,
                    },
                    start: {
                      character: 2,
                      line: 10,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: '#foo',
              },
              {
                kind: 5,
                location: {
                  range: {
                    end: {
                      character: 2,
                      line: 23,
                    },
                    start: {
                      character: 6,
                      line: 21,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'z',
              },
              {
                containerName: 'z',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 10,
                      line: 22,
                    },
                    start: {
                      character: 2,
                      line: 22,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 5,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 27,
                    },
                    start: {
                      character: 6,
                      line: 25,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'Z1',
              },
              {
                containerName: 'Z1',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 10,
                      line: 26,
                    },
                    start: {
                      character: 2,
                      line: 26,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 5,
                location: {
                  range: {
                    end: {
                      character: 2,
                      line: 31,
                    },
                    start: {
                      character: 6,
                      line: 29,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'Z2',
              },
              {
                containerName: 'Z2',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 10,
                      line: 30,
                    },
                    start: {
                      character: 2,
                      line: 30,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 5,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 35,
                    },
                    start: {
                      character: 6,
                      line: 33,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'Z3',
              },
              {
                containerName: 'Z3',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 10,
                      line: 34,
                    },
                    start: {
                      character: 2,
                      line: 34,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 40,
                    },
                    start: {
                      character: 6,
                      line: 38,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'z4',
              },
              {
                containerName: 'z4',
                kind: 5,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 40,
                    },
                    start: {
                      character: 11,
                      line: 38,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'Z4',
              },
              {
                containerName: 'Z4',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 10,
                      line: 39,
                    },
                    start: {
                      character: 2,
                      line: 39,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 5,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 44,
                    },
                    start: {
                      character: 0,
                      line: 42,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'Z5',
              },
              {
                containerName: 'Z5',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 13,
                      line: 43,
                    },
                    start: {
                      character: 2,
                      line: 43,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 5,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 48,
                    },
                    start: {
                      character: 15,
                      line: 46,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'Z6',
              },
              {
                containerName: 'Z6',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 13,
                      line: 47,
                    },
                    start: {
                      character: 2,
                      line: 47,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 5,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 52,
                    },
                    start: {
                      character: 7,
                      line: 50,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'Z7',
              },
              {
                containerName: 'Z7',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 10,
                      line: 51,
                    },
                    start: {
                      character: 2,
                      line: 51,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 12,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 56,
                    },
                    start: {
                      character: 0,
                      line: 54,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'f1',
              },
              {
                containerName: 'f1',
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 18,
                      line: 55,
                    },
                    start: {
                      character: 8,
                      line: 55,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'f1_x',
              },
              {
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 60,
                    },
                    start: {
                      character: 6,
                      line: 58,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'f2',
              },
              {
                containerName: 'f2',
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 18,
                      line: 59,
                    },
                    start: {
                      character: 8,
                      line: 59,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'f2_x',
              },
              {
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 64,
                    },
                    start: {
                      character: 6,
                      line: 62,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'f3',
              },
              {
                containerName: 'f3',
                kind: 12,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 64,
                    },
                    start: {
                      character: 11,
                      line: 62,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'f3_expr',
              },
              {
                containerName: 'f3_expr',
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 18,
                      line: 63,
                    },
                    start: {
                      character: 8,
                      line: 63,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'f3_x',
              },
              {
                kind: 12,
                location: {
                  range: {
                    end: {
                      character: 28,
                      line: 66,
                    },
                    start: {
                      character: 0,
                      line: 66,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'f4',
              },
              {
                kind: 12,
                location: {
                  range: {
                    end: {
                      character: 35,
                      line: 68,
                    },
                    start: {
                      character: 15,
                      line: 68,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'f5',
              },
              {
                kind: 12,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 72,
                    },
                    start: {
                      character: 7,
                      line: 70,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'f6',
              },
              {
                containerName: 'f6',
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 18,
                      line: 71,
                    },
                    start: {
                      character: 8,
                      line: 71,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'f6_x',
              },
              {
                kind: 12,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 76,
                    },
                    start: {
                      character: 1,
                      line: 74,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: '<function>',
              },
              {
                containerName: '<function>',
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 20,
                      line: 75,
                    },
                    start: {
                      character: 8,
                      line: 75,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'iife_x',
              },
              {
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 83,
                    },
                    start: {
                      character: 0,
                      line: 78,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'T1',
              },
              {
                containerName: 'T1',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 16,
                      line: 82,
                    },
                    start: {
                      character: 2,
                      line: 82,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: '[[call]]',
              },
              {
                containerName: 'T1',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 23,
                      line: 81,
                    },
                    start: {
                      character: 2,
                      line: 81,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: '[key]',
              },
              {
                containerName: 'T1',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 22,
                      line: 80,
                    },
                    start: {
                      character: 2,
                      line: 80,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'bar',
              },
              {
                containerName: 'bar',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 20,
                      line: 80,
                    },
                    start: {
                      character: 9,
                      line: 80,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'baz',
              },
              {
                containerName: 'T1',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 13,
                      line: 79,
                    },
                    start: {
                      character: 2,
                      line: 79,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 11,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 89,
                    },
                    start: {
                      character: 0,
                      line: 85,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'I1',
              },
              {
                containerName: 'I1',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 23,
                      line: 88,
                    },
                    start: {
                      character: 2,
                      line: 88,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: '[key]',
              },
              {
                containerName: 'I1',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 22,
                      line: 87,
                    },
                    start: {
                      character: 2,
                      line: 87,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'bar',
              },
              {
                containerName: 'bar',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 20,
                      line: 87,
                    },
                    start: {
                      character: 9,
                      line: 87,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'baz',
              },
              {
                containerName: 'I1',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 13,
                      line: 86,
                    },
                    start: {
                      character: 2,
                      line: 86,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 11,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 93,
                    },
                    start: {
                      character: 0,
                      line: 91,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'I2',
              },
              {
                containerName: 'I2',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 13,
                      line: 92,
                    },
                    start: {
                      character: 2,
                      line: 92,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 97,
                    },
                    start: {
                      character: 0,
                      line: 95,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'I3',
              },
              {
                containerName: 'I3',
                kind: 6,
                location: {
                  range: {
                    end: {
                      character: 13,
                      line: 96,
                    },
                    start: {
                      character: 2,
                      line: 96,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 101,
                    },
                    start: {
                      character: 0,
                      line: 99,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'I4',
              },
              {
                containerName: 'I4',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 13,
                      line: 100,
                    },
                    start: {
                      character: 2,
                      line: 100,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 105,
                    },
                    start: {
                      character: 0,
                      line: 103,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'I5',
              },
              {
                containerName: 'I5',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 13,
                      line: 104,
                    },
                    start: {
                      character: 2,
                      line: 104,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 35,
                      line: 107,
                    },
                    start: {
                      character: 0,
                      line: 107,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'decl1',
              },
              {
                containerName: 'decl1',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 32,
                      line: 107,
                    },
                    start: {
                      character: 21,
                      line: 107,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 42,
                      line: 109,
                    },
                    start: {
                      character: 15,
                      line: 109,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'decl2',
              },
              {
                containerName: 'decl2',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 39,
                      line: 109,
                    },
                    start: {
                      character: 28,
                      line: 109,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 36,
                      line: 111,
                    },
                    start: {
                      character: 25,
                      line: 111,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 2,
                      line: 115,
                    },
                    start: {
                      character: 0,
                      line: 113,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'default',
              },
              {
                containerName: 'default',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 10,
                      line: 114,
                    },
                    start: {
                      character: 2,
                      line: 114,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'foo',
              },
              {
                kind: 2,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 119,
                    },
                    start: {
                      character: 0,
                      line: 117,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'M1',
              },
              {
                containerName: 'M1',
                kind: 5,
                location: {
                  range: {
                    end: {
                      character: 20,
                      line: 118,
                    },
                    start: {
                      character: 2,
                      line: 118,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'C',
              },
              {
                kind: 2,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 123,
                    },
                    start: {
                      character: 0,
                      line: 121,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: '"M2"',
              },
              {
                containerName: '"M2"',
                kind: 5,
                location: {
                  range: {
                    end: {
                      character: 20,
                      line: 122,
                    },
                    start: {
                      character: 2,
                      line: 122,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'C',
              },
              {
                kind: 10,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 127,
                    },
                    start: {
                      character: 0,
                      line: 125,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'E1',
              },
              {
                containerName: 'E1',
                kind: 22,
                location: {
                  range: {
                    end: {
                      character: 3,
                      line: 126,
                    },
                    start: {
                      character: 2,
                      line: 126,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'X',
              },
              {
                kind: 12,
                location: {
                  range: {
                    end: {
                      character: 1,
                      line: 133,
                    },
                    start: {
                      character: 0,
                      line: 129,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'loops',
              },
              {
                containerName: 'loops',
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 12,
                      line: 132,
                    },
                    start: {
                      character: 11,
                      line: 132,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'k',
              },
              {
                containerName: 'loops',
                kind: 7,
                location: {
                  range: {
                    end: {
                      character: 24,
                      line: 131,
                    },
                    start: {
                      character: 18,
                      line: 131,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'x',
              },
              {
                containerName: 'loops',
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 12,
                      line: 131,
                    },
                    start: {
                      character: 11,
                      line: 131,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'j',
              },
              {
                containerName: 'loops',
                kind: 13,
                location: {
                  range: {
                    end: {
                      character: 16,
                      line: 130,
                    },
                    start: {
                      character: 11,
                      line: 130,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'i',
              },
              {
                kind: 12,
                location: {
                  range: {
                    end: {
                      character: 27,
                      line: 135,
                    },
                    start: {
                      character: 0,
                      line: 135,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'ComponentFoo',
              },
              {
                kind: 12,
                location: {
                  range: {
                    end: {
                      character: 33,
                      line: 136,
                    },
                    start: {
                      character: 0,
                      line: 136,
                    },
                  },
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                },
                name: 'ComponentBar',
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
): SuiteType);
