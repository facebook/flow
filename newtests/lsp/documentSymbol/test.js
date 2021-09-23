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
                kind: 13,
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
                selectionRange: {
                  start: {
                    line: 2,
                    character: 6,
                  },
                  end: {
                    line: 2,
                    character: 7,
                  },
                },
                children: [
                  {
                    name: 'obj',
                    kind: 7,
                    range: {
                      start: {
                        line: 6,
                        character: 2,
                      },
                      end: {
                        line: 6,
                        character: 24,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 6,
                        character: 2,
                      },
                      end: {
                        line: 6,
                        character: 5,
                      },
                    },
                    children: [
                      {
                        name: 'nested',
                        kind: 7,
                        range: {
                          start: {
                            line: 6,
                            character: 9,
                          },
                          end: {
                            line: 6,
                            character: 22,
                          },
                        },
                        selectionRange: {
                          start: {
                            line: 6,
                            character: 9,
                          },
                          end: {
                            line: 6,
                            character: 15,
                          },
                        },
                      },
                    ],
                  },
                  {
                    name: 'bar',
                    kind: 6,
                    range: {
                      start: {
                        line: 5,
                        character: 2,
                      },
                      end: {
                        line: 5,
                        character: 20,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 5,
                        character: 2,
                      },
                      end: {
                        line: 5,
                        character: 5,
                      },
                    },
                  },
                  {
                    name: 'foo',
                    kind: 7,
                    range: {
                      start: {
                        line: 4,
                        character: 2,
                      },
                      end: {
                        line: 4,
                        character: 10,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 4,
                        character: 2,
                      },
                      end: {
                        line: 4,
                        character: 5,
                      },
                    },
                  },
                  {
                    name: 'C',
                    kind: 5,
                    range: {
                      start: {
                        line: 3,
                        character: 2,
                      },
                      end: {
                        line: 3,
                        character: 13,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 3,
                        character: 2,
                      },
                      end: {
                        line: 3,
                        character: 3,
                      },
                    },
                  },
                ],
              },
              {
                name: 'Y',
                kind: 5,
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
                selectionRange: {
                  start: {
                    line: 9,
                    character: 6,
                  },
                  end: {
                    line: 9,
                    character: 7,
                  },
                },
                children: [
                  {
                    name: 'Z',
                    kind: 5,
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
                    selectionRange: {
                      start: {
                        line: 16,
                        character: 2,
                      },
                      end: {
                        line: 16,
                        character: 3,
                      },
                    },
                    children: [
                      {
                        name: 'zmeth',
                        kind: 6,
                        range: {
                          start: {
                            line: 17,
                            character: 4,
                          },
                          end: {
                            line: 17,
                            character: 14,
                          },
                        },
                        selectionRange: {
                          start: {
                            line: 17,
                            character: 4,
                          },
                          end: {
                            line: 17,
                            character: 9,
                          },
                        },
                      },
                    ],
                  },
                  {
                    name: '(set) abc',
                    kind: 7,
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
                    selectionRange: {
                      start: {
                        line: 15,
                        character: 6,
                      },
                      end: {
                        line: 15,
                        character: 9,
                      },
                    },
                  },
                  {
                    name: '(get) abc',
                    kind: 7,
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
                    selectionRange: {
                      start: {
                        line: 14,
                        character: 6,
                      },
                      end: {
                        line: 14,
                        character: 9,
                      },
                    },
                  },
                  {
                    name: '#baz',
                    kind: 7,
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
                    selectionRange: {
                      start: {
                        line: 13,
                        character: 2,
                      },
                      end: {
                        line: 13,
                        character: 6,
                      },
                    },
                  },
                  {
                    name: 'bar',
                    kind: 7,
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
                    selectionRange: {
                      start: {
                        line: 12,
                        character: 2,
                      },
                      end: {
                        line: 12,
                        character: 5,
                      },
                    },
                  },
                  {
                    name: 'foo',
                    kind: 6,
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
                    selectionRange: {
                      start: {
                        line: 11,
                        character: 2,
                      },
                      end: {
                        line: 11,
                        character: 5,
                      },
                    },
                  },
                  {
                    name: '#foo',
                    kind: 6,
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
                    selectionRange: {
                      start: {
                        line: 10,
                        character: 2,
                      },
                      end: {
                        line: 10,
                        character: 6,
                      },
                    },
                  },
                ],
              },
              {
                name: 'z',
                kind: 5,
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
                selectionRange: {
                  start: {
                    line: 21,
                    character: 6,
                  },
                  end: {
                    line: 21,
                    character: 7,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 6,
                    range: {
                      start: {
                        line: 22,
                        character: 2,
                      },
                      end: {
                        line: 22,
                        character: 10,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 22,
                        character: 2,
                      },
                      end: {
                        line: 22,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'Z1',
                kind: 5,
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
                selectionRange: {
                  start: {
                    line: 25,
                    character: 6,
                  },
                  end: {
                    line: 25,
                    character: 8,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 6,
                    range: {
                      start: {
                        line: 26,
                        character: 2,
                      },
                      end: {
                        line: 26,
                        character: 10,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 26,
                        character: 2,
                      },
                      end: {
                        line: 26,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'Z2',
                kind: 5,
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
                selectionRange: {
                  start: {
                    line: 29,
                    character: 6,
                  },
                  end: {
                    line: 29,
                    character: 8,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 6,
                    range: {
                      start: {
                        line: 30,
                        character: 2,
                      },
                      end: {
                        line: 30,
                        character: 10,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 30,
                        character: 2,
                      },
                      end: {
                        line: 30,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'Z3',
                kind: 5,
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
                selectionRange: {
                  start: {
                    line: 33,
                    character: 6,
                  },
                  end: {
                    line: 33,
                    character: 8,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 6,
                    range: {
                      start: {
                        line: 34,
                        character: 2,
                      },
                      end: {
                        line: 34,
                        character: 10,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 34,
                        character: 2,
                      },
                      end: {
                        line: 34,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'z4',
                kind: 13,
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
                selectionRange: {
                  start: {
                    line: 38,
                    character: 6,
                  },
                  end: {
                    line: 38,
                    character: 8,
                  },
                },
                children: [
                  {
                    name: 'Z4',
                    kind: 5,
                    range: {
                      start: {
                        line: 38,
                        character: 11,
                      },
                      end: {
                        line: 40,
                        character: 1,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 38,
                        character: 17,
                      },
                      end: {
                        line: 38,
                        character: 19,
                      },
                    },
                    children: [
                      {
                        name: 'foo',
                        kind: 6,
                        range: {
                          start: {
                            line: 39,
                            character: 2,
                          },
                          end: {
                            line: 39,
                            character: 10,
                          },
                        },
                        selectionRange: {
                          start: {
                            line: 39,
                            character: 2,
                          },
                          end: {
                            line: 39,
                            character: 5,
                          },
                        },
                      },
                    ],
                  },
                ],
              },
              {
                name: 'Z5',
                kind: 5,
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
                selectionRange: {
                  start: {
                    line: 42,
                    character: 14,
                  },
                  end: {
                    line: 42,
                    character: 16,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 6,
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
                    selectionRange: {
                      start: {
                        line: 43,
                        character: 2,
                      },
                      end: {
                        line: 43,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'Z6',
                kind: 5,
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
                selectionRange: {
                  start: {
                    line: 46,
                    character: 21,
                  },
                  end: {
                    line: 46,
                    character: 23,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 6,
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
                    selectionRange: {
                      start: {
                        line: 47,
                        character: 2,
                      },
                      end: {
                        line: 47,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'Z7',
                kind: 5,
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
                selectionRange: {
                  start: {
                    line: 50,
                    character: 13,
                  },
                  end: {
                    line: 50,
                    character: 15,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 6,
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
                    selectionRange: {
                      start: {
                        line: 51,
                        character: 2,
                      },
                      end: {
                        line: 51,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'f1',
                kind: 12,
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
                selectionRange: {
                  start: {
                    line: 54,
                    character: 9,
                  },
                  end: {
                    line: 54,
                    character: 11,
                  },
                },
                children: [
                  {
                    name: 'f1_x',
                    kind: 13,
                    range: {
                      start: {
                        line: 55,
                        character: 8,
                      },
                      end: {
                        line: 55,
                        character: 18,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 55,
                        character: 8,
                      },
                      end: {
                        line: 55,
                        character: 12,
                      },
                    },
                  },
                ],
              },
              {
                name: 'f2',
                kind: 6,
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
                selectionRange: {
                  start: {
                    line: 58,
                    character: 6,
                  },
                  end: {
                    line: 58,
                    character: 8,
                  },
                },
                children: [
                  {
                    name: 'f2_x',
                    kind: 13,
                    range: {
                      start: {
                        line: 59,
                        character: 8,
                      },
                      end: {
                        line: 59,
                        character: 18,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 59,
                        character: 8,
                      },
                      end: {
                        line: 59,
                        character: 12,
                      },
                    },
                  },
                ],
              },
              {
                name: 'f3',
                kind: 13,
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
                selectionRange: {
                  start: {
                    line: 62,
                    character: 6,
                  },
                  end: {
                    line: 62,
                    character: 8,
                  },
                },
                children: [
                  {
                    name: 'f3_expr',
                    kind: 12,
                    range: {
                      start: {
                        line: 62,
                        character: 11,
                      },
                      end: {
                        line: 64,
                        character: 1,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 62,
                        character: 20,
                      },
                      end: {
                        line: 62,
                        character: 27,
                      },
                    },
                    children: [
                      {
                        name: 'f3_x',
                        kind: 13,
                        range: {
                          start: {
                            line: 63,
                            character: 8,
                          },
                          end: {
                            line: 63,
                            character: 18,
                          },
                        },
                        selectionRange: {
                          start: {
                            line: 63,
                            character: 8,
                          },
                          end: {
                            line: 63,
                            character: 12,
                          },
                        },
                      },
                    ],
                  },
                ],
              },
              {
                name: 'f4',
                kind: 12,
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
                selectionRange: {
                  start: {
                    line: 66,
                    character: 17,
                  },
                  end: {
                    line: 66,
                    character: 19,
                  },
                },
              },
              {
                name: 'f5',
                kind: 12,
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
                selectionRange: {
                  start: {
                    line: 68,
                    character: 24,
                  },
                  end: {
                    line: 68,
                    character: 26,
                  },
                },
              },
              {
                name: 'f6',
                kind: 12,
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
                selectionRange: {
                  start: {
                    line: 70,
                    character: 16,
                  },
                  end: {
                    line: 70,
                    character: 18,
                  },
                },
                children: [
                  {
                    name: 'f6_x',
                    kind: 13,
                    range: {
                      start: {
                        line: 71,
                        character: 8,
                      },
                      end: {
                        line: 71,
                        character: 18,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 71,
                        character: 8,
                      },
                      end: {
                        line: 71,
                        character: 12,
                      },
                    },
                  },
                ],
              },
              {
                name: '<function>',
                kind: 12,
                range: {
                  start: {
                    line: 74,
                    character: 1,
                  },
                  end: {
                    line: 76,
                    character: 1,
                  },
                },
                selectionRange: {
                  start: {
                    line: 74,
                    character: 1,
                  },
                  end: {
                    line: 76,
                    character: 1,
                  },
                },
                children: [
                  {
                    name: 'iife_x',
                    kind: 13,
                    range: {
                      start: {
                        line: 75,
                        character: 8,
                      },
                      end: {
                        line: 75,
                        character: 20,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 75,
                        character: 8,
                      },
                      end: {
                        line: 75,
                        character: 14,
                      },
                    },
                  },
                ],
              },
              {
                name: 'T1',
                kind: 13,
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
                selectionRange: {
                  start: {
                    line: 78,
                    character: 5,
                  },
                  end: {
                    line: 78,
                    character: 7,
                  },
                },
                children: [
                  {
                    name: '[[call]]',
                    kind: 7,
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
                    selectionRange: {
                      start: {
                        line: 82,
                        character: 4,
                      },
                      end: {
                        line: 82,
                        character: 8,
                      },
                    },
                  },
                  {
                    name: '[key]',
                    kind: 7,
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
                    selectionRange: {
                      start: {
                        line: 81,
                        character: 3,
                      },
                      end: {
                        line: 81,
                        character: 6,
                      },
                    },
                  },
                  {
                    name: 'bar',
                    kind: 7,
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
                    selectionRange: {
                      start: {
                        line: 80,
                        character: 2,
                      },
                      end: {
                        line: 80,
                        character: 5,
                      },
                    },
                    children: [
                      {
                        name: 'baz',
                        kind: 7,
                        range: {
                          start: {
                            line: 80,
                            character: 9,
                          },
                          end: {
                            line: 80,
                            character: 20,
                          },
                        },
                        selectionRange: {
                          start: {
                            line: 80,
                            character: 9,
                          },
                          end: {
                            line: 80,
                            character: 12,
                          },
                        },
                      },
                    ],
                  },
                  {
                    name: 'foo',
                    kind: 6,
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
                    selectionRange: {
                      start: {
                        line: 79,
                        character: 2,
                      },
                      end: {
                        line: 79,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'I1',
                kind: 11,
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
                selectionRange: {
                  start: {
                    line: 85,
                    character: 10,
                  },
                  end: {
                    line: 85,
                    character: 12,
                  },
                },
                children: [
                  {
                    name: '[key]',
                    kind: 7,
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
                    selectionRange: {
                      start: {
                        line: 88,
                        character: 3,
                      },
                      end: {
                        line: 88,
                        character: 6,
                      },
                    },
                  },
                  {
                    name: 'bar',
                    kind: 7,
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
                    selectionRange: {
                      start: {
                        line: 87,
                        character: 2,
                      },
                      end: {
                        line: 87,
                        character: 5,
                      },
                    },
                    children: [
                      {
                        name: 'baz',
                        kind: 7,
                        range: {
                          start: {
                            line: 87,
                            character: 9,
                          },
                          end: {
                            line: 87,
                            character: 20,
                          },
                        },
                        selectionRange: {
                          start: {
                            line: 87,
                            character: 9,
                          },
                          end: {
                            line: 87,
                            character: 12,
                          },
                        },
                      },
                    ],
                  },
                  {
                    name: 'foo',
                    kind: 6,
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
                    selectionRange: {
                      start: {
                        line: 86,
                        character: 2,
                      },
                      end: {
                        line: 86,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'I2',
                kind: 11,
                range: {
                  start: {
                    line: 91,
                    character: 0,
                  },
                  end: {
                    line: 93,
                    character: 1,
                  },
                },
                selectionRange: {
                  start: {
                    line: 91,
                    character: 18,
                  },
                  end: {
                    line: 91,
                    character: 20,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 6,
                    range: {
                      start: {
                        line: 92,
                        character: 2,
                      },
                      end: {
                        line: 92,
                        character: 13,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 92,
                        character: 2,
                      },
                      end: {
                        line: 92,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'I3',
                kind: 13,
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
                selectionRange: {
                  start: {
                    line: 95,
                    character: 5,
                  },
                  end: {
                    line: 95,
                    character: 7,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 6,
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
                    selectionRange: {
                      start: {
                        line: 96,
                        character: 2,
                      },
                      end: {
                        line: 96,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'I4',
                kind: 13,
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
                selectionRange: {
                  start: {
                    line: 99,
                    character: 12,
                  },
                  end: {
                    line: 99,
                    character: 14,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 7,
                    range: {
                      start: {
                        line: 100,
                        character: 2,
                      },
                      end: {
                        line: 100,
                        character: 13,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 100,
                        character: 2,
                      },
                      end: {
                        line: 100,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'I5',
                kind: 13,
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
                selectionRange: {
                  start: {
                    line: 103,
                    character: 20,
                  },
                  end: {
                    line: 103,
                    character: 22,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 7,
                    range: {
                      start: {
                        line: 104,
                        character: 2,
                      },
                      end: {
                        line: 104,
                        character: 13,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 104,
                        character: 2,
                      },
                      end: {
                        line: 104,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'decl1',
                kind: 13,
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
                selectionRange: {
                  start: {
                    line: 107,
                    character: 12,
                  },
                  end: {
                    line: 107,
                    character: 17,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 7,
                    range: {
                      start: {
                        line: 107,
                        character: 21,
                      },
                      end: {
                        line: 107,
                        character: 32,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 107,
                        character: 21,
                      },
                      end: {
                        line: 107,
                        character: 24,
                      },
                    },
                  },
                ],
              },
              {
                name: 'decl2',
                kind: 13,
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
                selectionRange: {
                  start: {
                    line: 109,
                    character: 19,
                  },
                  end: {
                    line: 109,
                    character: 24,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 7,
                    range: {
                      start: {
                        line: 109,
                        character: 28,
                      },
                      end: {
                        line: 109,
                        character: 39,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 109,
                        character: 28,
                      },
                      end: {
                        line: 109,
                        character: 31,
                      },
                    },
                  },
                ],
              },
              {
                name: 'foo',
                kind: 7,
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
                selectionRange: {
                  start: {
                    line: 111,
                    character: 25,
                  },
                  end: {
                    line: 111,
                    character: 28,
                  },
                },
              },
              {
                name: 'default',
                kind: 13,
                range: {
                  start: {
                    line: 113,
                    character: 0,
                  },
                  end: {
                    line: 115,
                    character: 2,
                  },
                },
                selectionRange: {
                  start: {
                    line: 113,
                    character: 7,
                  },
                  end: {
                    line: 113,
                    character: 14,
                  },
                },
                children: [
                  {
                    name: 'foo',
                    kind: 7,
                    range: {
                      start: {
                        line: 114,
                        character: 2,
                      },
                      end: {
                        line: 114,
                        character: 10,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 114,
                        character: 2,
                      },
                      end: {
                        line: 114,
                        character: 5,
                      },
                    },
                  },
                ],
              },
              {
                name: 'M1',
                kind: 2,
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
                selectionRange: {
                  start: {
                    line: 117,
                    character: 15,
                  },
                  end: {
                    line: 117,
                    character: 17,
                  },
                },
                children: [
                  {
                    name: 'C',
                    kind: 5,
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
                    selectionRange: {
                      start: {
                        line: 118,
                        character: 16,
                      },
                      end: {
                        line: 118,
                        character: 17,
                      },
                    },
                  },
                ],
              },
              {
                name: '"M2"',
                kind: 2,
                range: {
                  start: {
                    line: 121,
                    character: 0,
                  },
                  end: {
                    line: 123,
                    character: 1,
                  },
                },
                selectionRange: {
                  start: {
                    line: 121,
                    character: 15,
                  },
                  end: {
                    line: 121,
                    character: 19,
                  },
                },
                children: [
                  {
                    name: 'C',
                    kind: 5,
                    range: {
                      start: {
                        line: 122,
                        character: 2,
                      },
                      end: {
                        line: 122,
                        character: 20,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 122,
                        character: 16,
                      },
                      end: {
                        line: 122,
                        character: 17,
                      },
                    },
                  },
                ],
              },
              {
                name: 'E1',
                kind: 10,
                range: {
                  start: {
                    line: 125,
                    character: 0,
                  },
                  end: {
                    line: 127,
                    character: 1,
                  },
                },
                selectionRange: {
                  start: {
                    line: 125,
                    character: 5,
                  },
                  end: {
                    line: 125,
                    character: 7,
                  },
                },
                children: [
                  {
                    name: 'X',
                    kind: 22,
                    range: {
                      start: {
                        line: 126,
                        character: 2,
                      },
                      end: {
                        line: 126,
                        character: 3,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 126,
                        character: 2,
                      },
                      end: {
                        line: 126,
                        character: 3,
                      },
                    },
                  },
                ],
              },
              {
                name: 'loops',
                kind: 12,
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
                selectionRange: {
                  start: {
                    line: 129,
                    character: 9,
                  },
                  end: {
                    line: 129,
                    character: 14,
                  },
                },
                children: [
                  {
                    name: 'k',
                    kind: 13,
                    range: {
                      start: {
                        line: 132,
                        character: 11,
                      },
                      end: {
                        line: 132,
                        character: 12,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 132,
                        character: 11,
                      },
                      end: {
                        line: 132,
                        character: 12,
                      },
                    },
                  },
                  {
                    name: 'x',
                    kind: 7,
                    range: {
                      start: {
                        line: 131,
                        character: 18,
                      },
                      end: {
                        line: 131,
                        character: 24,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 131,
                        character: 18,
                      },
                      end: {
                        line: 131,
                        character: 19,
                      },
                    },
                  },
                  {
                    name: 'j',
                    kind: 13,
                    range: {
                      start: {
                        line: 131,
                        character: 11,
                      },
                      end: {
                        line: 131,
                        character: 12,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 131,
                        character: 11,
                      },
                      end: {
                        line: 131,
                        character: 12,
                      },
                    },
                  },
                  {
                    name: 'i',
                    kind: 13,
                    range: {
                      start: {
                        line: 130,
                        character: 11,
                      },
                      end: {
                        line: 130,
                        character: 16,
                      },
                    },
                    selectionRange: {
                      start: {
                        line: 130,
                        character: 11,
                      },
                      end: {
                        line: 130,
                        character: 12,
                      },
                    },
                  },
                ],
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
                kind: 13,
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
                name: 'obj',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 6,
                      character: 2,
                    },
                    end: {
                      line: 6,
                      character: 24,
                    },
                  },
                },
                containerName: 'x',
              },
              {
                name: 'nested',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 6,
                      character: 9,
                    },
                    end: {
                      line: 6,
                      character: 22,
                    },
                  },
                },
                containerName: 'obj',
              },
              {
                name: 'bar',
                kind: 6,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 5,
                      character: 2,
                    },
                    end: {
                      line: 5,
                      character: 20,
                    },
                  },
                },
                containerName: 'x',
              },
              {
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 4,
                      character: 2,
                    },
                    end: {
                      line: 4,
                      character: 10,
                    },
                  },
                },
                containerName: 'x',
              },
              {
                name: 'C',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 3,
                      character: 2,
                    },
                    end: {
                      line: 3,
                      character: 13,
                    },
                  },
                },
                containerName: 'x',
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
                name: 'Z',
                kind: 5,
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
                name: 'zmeth',
                kind: 6,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 17,
                      character: 4,
                    },
                    end: {
                      line: 17,
                      character: 14,
                    },
                  },
                },
                containerName: 'Z',
              },
              {
                name: '(set) abc',
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
                name: '(get) abc',
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
                name: '#baz',
                kind: 7,
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
                name: '#foo',
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
                name: 'z',
                kind: 5,
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
                name: 'foo',
                kind: 6,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 22,
                      character: 2,
                    },
                    end: {
                      line: 22,
                      character: 10,
                    },
                  },
                },
                containerName: 'z',
              },
              {
                name: 'Z1',
                kind: 5,
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
                name: 'foo',
                kind: 6,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 26,
                      character: 2,
                    },
                    end: {
                      line: 26,
                      character: 10,
                    },
                  },
                },
                containerName: 'Z1',
              },
              {
                name: 'Z2',
                kind: 5,
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
                name: 'foo',
                kind: 6,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 30,
                      character: 2,
                    },
                    end: {
                      line: 30,
                      character: 10,
                    },
                  },
                },
                containerName: 'Z2',
              },
              {
                name: 'Z3',
                kind: 5,
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
                name: 'foo',
                kind: 6,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 34,
                      character: 2,
                    },
                    end: {
                      line: 34,
                      character: 10,
                    },
                  },
                },
                containerName: 'Z3',
              },
              {
                name: 'z4',
                kind: 13,
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
                name: 'Z4',
                kind: 5,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 38,
                      character: 11,
                    },
                    end: {
                      line: 40,
                      character: 1,
                    },
                  },
                },
                containerName: 'z4',
              },
              {
                name: 'foo',
                kind: 6,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 39,
                      character: 2,
                    },
                    end: {
                      line: 39,
                      character: 10,
                    },
                  },
                },
                containerName: 'Z4',
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
                kind: 6,
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
                kind: 6,
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
                name: 'f1_x',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 55,
                      character: 8,
                    },
                    end: {
                      line: 55,
                      character: 18,
                    },
                  },
                },
                containerName: 'f1',
              },
              {
                name: 'f2',
                kind: 6,
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
                name: 'f2_x',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 59,
                      character: 8,
                    },
                    end: {
                      line: 59,
                      character: 18,
                    },
                  },
                },
                containerName: 'f2',
              },
              {
                name: 'f3',
                kind: 13,
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
                name: 'f3_expr',
                kind: 12,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 62,
                      character: 11,
                    },
                    end: {
                      line: 64,
                      character: 1,
                    },
                  },
                },
                containerName: 'f3',
              },
              {
                name: 'f3_x',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 63,
                      character: 8,
                    },
                    end: {
                      line: 63,
                      character: 18,
                    },
                  },
                },
                containerName: 'f3_expr',
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
                name: 'f6_x',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 71,
                      character: 8,
                    },
                    end: {
                      line: 71,
                      character: 18,
                    },
                  },
                },
                containerName: 'f6',
              },
              {
                name: '<function>',
                kind: 12,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 74,
                      character: 1,
                    },
                    end: {
                      line: 76,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'iife_x',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 75,
                      character: 8,
                    },
                    end: {
                      line: 75,
                      character: 20,
                    },
                  },
                },
                containerName: '<function>',
              },
              {
                name: 'T1',
                kind: 13,
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
                name: '[[call]]',
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
                name: '[key]',
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
                name: 'baz',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 80,
                      character: 9,
                    },
                    end: {
                      line: 80,
                      character: 20,
                    },
                  },
                },
                containerName: 'bar',
              },
              {
                name: 'foo',
                kind: 6,
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
                name: '[key]',
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
                name: 'baz',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 87,
                      character: 9,
                    },
                    end: {
                      line: 87,
                      character: 20,
                    },
                  },
                },
                containerName: 'bar',
              },
              {
                name: 'foo',
                kind: 6,
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
                name: 'I2',
                kind: 11,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 91,
                      character: 0,
                    },
                    end: {
                      line: 93,
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
                      line: 92,
                      character: 2,
                    },
                    end: {
                      line: 92,
                      character: 13,
                    },
                  },
                },
                containerName: 'I2',
              },
              {
                name: 'I3',
                kind: 13,
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
                kind: 6,
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
                kind: 13,
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
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 100,
                      character: 2,
                    },
                    end: {
                      line: 100,
                      character: 13,
                    },
                  },
                },
                containerName: 'I4',
              },
              {
                name: 'I5',
                kind: 13,
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
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 104,
                      character: 2,
                    },
                    end: {
                      line: 104,
                      character: 13,
                    },
                  },
                },
                containerName: 'I5',
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
                name: 'foo',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 107,
                      character: 21,
                    },
                    end: {
                      line: 107,
                      character: 32,
                    },
                  },
                },
                containerName: 'decl1',
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
                      line: 109,
                      character: 28,
                    },
                    end: {
                      line: 109,
                      character: 39,
                    },
                  },
                },
                containerName: 'decl2',
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
                name: 'default',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 113,
                      character: 0,
                    },
                    end: {
                      line: 115,
                      character: 2,
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
                      line: 114,
                      character: 2,
                    },
                    end: {
                      line: 114,
                      character: 10,
                    },
                  },
                },
                containerName: 'default',
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
                name: '"M2"',
                kind: 2,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 121,
                      character: 0,
                    },
                    end: {
                      line: 123,
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
                      line: 122,
                      character: 2,
                    },
                    end: {
                      line: 122,
                      character: 20,
                    },
                  },
                },
                containerName: '"M2"',
              },
              {
                name: 'E1',
                kind: 10,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 125,
                      character: 0,
                    },
                    end: {
                      line: 127,
                      character: 1,
                    },
                  },
                },
              },
              {
                name: 'X',
                kind: 22,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 126,
                      character: 2,
                    },
                    end: {
                      line: 126,
                      character: 3,
                    },
                  },
                },
                containerName: 'E1',
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
              {
                name: 'k',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 132,
                      character: 11,
                    },
                    end: {
                      line: 132,
                      character: 12,
                    },
                  },
                },
                containerName: 'loops',
              },
              {
                name: 'x',
                kind: 7,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 131,
                      character: 18,
                    },
                    end: {
                      line: 131,
                      character: 24,
                    },
                  },
                },
                containerName: 'loops',
              },
              {
                name: 'j',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 131,
                      character: 11,
                    },
                    end: {
                      line: 131,
                      character: 12,
                    },
                  },
                },
                containerName: 'loops',
              },
              {
                name: 'i',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/stuff.js',
                  range: {
                    start: {
                      line: 130,
                      character: 11,
                    },
                    end: {
                      line: 130,
                      character: 16,
                    },
                  },
                },
                containerName: 'loops',
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
