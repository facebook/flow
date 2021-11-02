/*
 * @flow
 * @noformat
 */

import type {Suite} from 'flow-dev-tools/src/test/Suite';
import type LSPMessage from 'flow-dev-tools/src/test/lsp';
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(
  ({
    addFile,
    addFiles,
    addCode,
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    lspIgnoreStatusAndCancellation,
  }) => {
    let requestSelectionRange = function(
      uri: string,
      positions: Array<{|line: number, character: number|}>,
    ) {
      return lspRequestAndWaitUntilResponse('textDocument/selectionRange', {
        textDocument: {uri: uri},
        positions: positions,
      });
    };
    return [
      test('textDocument/selectionRange on jsx', [
        addFiles('jsx.js'),
        lspStartAndConnect(),
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/jsx.js',
          [{line: 7, character: 9}], // inside `Text`
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 6,
                      character: 29,
                    },
                    end: {
                      line: 8,
                      character: 4,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 6,
                        character: 4,
                      },
                      end: {
                        line: 8,
                        character: 10,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 5,
                          character: 7,
                        },
                        end: {
                          line: 9,
                          character: 2,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 5,
                            character: 2,
                          },
                          end: {
                            line: 9,
                            character: 8,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 4,
                              character: 0,
                            },
                            end: {
                              line: 10,
                              character: 2,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 2,
                                character: 0,
                              },
                              end: {
                                line: 10,
                                character: 2,
                              },
                            },
                            parent: {
                              range: {
                                start: {
                                  line: 0,
                                  character: 0,
                                },
                                end: {
                                  line: 10,
                                  character: 2,
                                },
                              },
                            },
                          },
                        },
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
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/jsx.js',
          [{line: 6, character: 11}], // inside attribute name
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 6,
                      character: 9,
                    },
                    end: {
                      line: 6,
                      character: 12,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 6,
                        character: 9,
                      },
                      end: {
                        line: 6,
                        character: 18,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 6,
                          character: 4,
                        },
                        end: {
                          line: 6,
                          character: 29,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 6,
                            character: 4,
                          },
                          end: {
                            line: 8,
                            character: 10,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 5,
                              character: 7,
                            },
                            end: {
                              line: 9,
                              character: 2,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 5,
                                character: 2,
                              },
                              end: {
                                line: 9,
                                character: 8,
                              },
                            },
                            parent: {
                              range: {
                                start: {
                                  line: 4,
                                  character: 0,
                                },
                                end: {
                                  line: 10,
                                  character: 2,
                                },
                              },
                              parent: {
                                range: {
                                  start: {
                                    line: 2,
                                    character: 0,
                                  },
                                  end: {
                                    line: 10,
                                    character: 2,
                                  },
                                },
                                parent: {
                                  range: {
                                    start: {
                                      line: 0,
                                      character: 0,
                                    },
                                    end: {
                                      line: 10,
                                      character: 2,
                                    },
                                  },
                                },
                              },
                            },
                          },
                        },
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
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/jsx.js',
          [{line: 6, character: 15}], // inside attribute literal value
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 6,
                      character: 13,
                    },
                    end: {
                      line: 6,
                      character: 18,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 6,
                        character: 9,
                      },
                      end: {
                        line: 6,
                        character: 18,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 6,
                          character: 4,
                        },
                        end: {
                          line: 6,
                          character: 29,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 6,
                            character: 4,
                          },
                          end: {
                            line: 8,
                            character: 10,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 5,
                              character: 7,
                            },
                            end: {
                              line: 9,
                              character: 2,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 5,
                                character: 2,
                              },
                              end: {
                                line: 9,
                                character: 8,
                              },
                            },
                            parent: {
                              range: {
                                start: {
                                  line: 4,
                                  character: 0,
                                },
                                end: {
                                  line: 10,
                                  character: 2,
                                },
                              },
                              parent: {
                                range: {
                                  start: {
                                    line: 2,
                                    character: 0,
                                  },
                                  end: {
                                    line: 10,
                                    character: 2,
                                  },
                                },
                                parent: {
                                  range: {
                                    start: {
                                      line: 0,
                                      character: 0,
                                    },
                                    end: {
                                      line: 10,
                                      character: 2,
                                    },
                                  },
                                },
                              },
                            },
                          },
                        },
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
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/jsx.js',
          [{line: 6, character: 25}], // inside attribute expression
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 6,
                      character: 24,
                    },
                    end: {
                      line: 6,
                      character: 27,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 6,
                        character: 23,
                      },
                      end: {
                        line: 6,
                        character: 28,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 6,
                          character: 19,
                        },
                        end: {
                          line: 6,
                          character: 28,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 6,
                            character: 4,
                          },
                          end: {
                            line: 6,
                            character: 29,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 6,
                              character: 4,
                            },
                            end: {
                              line: 8,
                              character: 10,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 5,
                                character: 7,
                              },
                              end: {
                                line: 9,
                                character: 2,
                              },
                            },
                            parent: {
                              range: {
                                start: {
                                  line: 5,
                                  character: 2,
                                },
                                end: {
                                  line: 9,
                                  character: 8,
                                },
                              },
                              parent: {
                                range: {
                                  start: {
                                    line: 4,
                                    character: 0,
                                  },
                                  end: {
                                    line: 10,
                                    character: 2,
                                  },
                                },
                                parent: {
                                  range: {
                                    start: {
                                      line: 2,
                                      character: 0,
                                    },
                                    end: {
                                      line: 10,
                                      character: 2,
                                    },
                                  },
                                  parent: {
                                    range: {
                                      start: {
                                        line: 0,
                                        character: 0,
                                      },
                                      end: {
                                        line: 10,
                                        character: 2,
                                      },
                                    },
                                  },
                                },
                              },
                            },
                          },
                        },
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
      test('textDocument/selectionRange on array spread', [
        addFiles('array_spread.js'),
        lspStartAndConnect(),
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/array_spread.js',
          [{line: 4, character: 16}], // in baz
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 4,
                      character: 14,
                    },
                    end: {
                      line: 4,
                      character: 17,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 4,
                        character: 11,
                      },
                      end: {
                        line: 4,
                        character: 17,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 4,
                          character: 0,
                        },
                        end: {
                          line: 4,
                          character: 18,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 4,
                            character: 0,
                          },
                          end: {
                            line: 4,
                            character: 19,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 2,
                              character: 0,
                            },
                            end: {
                              line: 4,
                              character: 19,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 0,
                                character: 0,
                              },
                              end: {
                                line: 4,
                                character: 19,
                              },
                            },
                          },
                        },
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
      test('textDocument/selectionRange on functions', [
        addFiles('function.js'),
        lspStartAndConnect(),

        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/function.js',
          [{line: 3, character: 0}], // in declaration body
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 2,
                      character: 48,
                    },
                    end: {
                      line: 4,
                      character: 1,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 4,
                        character: 1,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 8,
                          character: 2,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 0,
                            character: 0,
                          },
                          end: {
                            line: 8,
                            character: 2,
                          },
                        },
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

        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/function.js',
          [{line: 7, character: 0}], // in expression body
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 6,
                      character: 30,
                    },
                    end: {
                      line: 8,
                      character: 1,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 6,
                        character: 1,
                      },
                      end: {
                        line: 8,
                        character: 1,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 6,
                          character: 0,
                        },
                        end: {
                          line: 8,
                          character: 2,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 2,
                            character: 0,
                          },
                          end: {
                            line: 8,
                            character: 2,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 0,
                              character: 0,
                            },
                            end: {
                              line: 8,
                              character: 2,
                            },
                          },
                        },
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

        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/function.js',
          [{line: 2, character: 31}], // in arg2 identifier
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 2,
                      character: 28,
                    },
                    end: {
                      line: 2,
                      character: 32,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 2,
                        character: 28,
                      },
                      end: {
                        line: 2,
                        character: 40,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 2,
                          character: 13,
                        },
                        end: {
                          line: 2,
                          character: 41,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 2,
                            character: 0,
                          },
                          end: {
                            line: 2,
                            character: 47,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 2,
                              character: 0,
                            },
                            end: {
                              line: 4,
                              character: 1,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 2,
                                character: 0,
                              },
                              end: {
                                line: 8,
                                character: 2,
                              },
                            },
                            parent: {
                              range: {
                                start: {
                                  line: 0,
                                  character: 0,
                                },
                                end: {
                                  line: 8,
                                  character: 2,
                                },
                              },
                            },
                          },
                        },
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
      test('textDocument/selectionRange on var', [
        addFiles('var.js'),
        lspStartAndConnect(),
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/var.js',
          [{line: 4, character: 15}], // inside 1234
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 4,
                      character: 12,
                    },
                    end: {
                      line: 4,
                      character: 16,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 4,
                        character: 4,
                      },
                      end: {
                        line: 4,
                        character: 16,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 4,
                          character: 0,
                        },
                        end: {
                          line: 4,
                          character: 17,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 2,
                            character: 0,
                          },
                          end: {
                            line: 4,
                            character: 17,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 0,
                              character: 0,
                            },
                            end: {
                              line: 4,
                              character: 17,
                            },
                          },
                        },
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
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/var.js',
          [{line: 4, character: 9}], // inside T of `var x : T`
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 4,
                      character: 8,
                    },
                    end: {
                      line: 4,
                      character: 9,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 4,
                        character: 6,
                      },
                      end: {
                        line: 4,
                        character: 9,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 4,
                          character: 4,
                        },
                        end: {
                          line: 4,
                          character: 9,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 4,
                            character: 4,
                          },
                          end: {
                            line: 4,
                            character: 16,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 4,
                              character: 0,
                            },
                            end: {
                              line: 4,
                              character: 17,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 2,
                                character: 0,
                              },
                              end: {
                                line: 4,
                                character: 17,
                              },
                            },
                            parent: {
                              range: {
                                start: {
                                  line: 0,
                                  character: 0,
                                },
                                end: {
                                  line: 4,
                                  character: 17,
                                },
                              },
                            },
                          },
                        },
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
      test('textDocument/selectionRange on declare export', [
        addFiles('declare_export.js'),
        lspStartAndConnect(),
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/declare_export.js',
          [{line: 2, character: 17}], // inside `var` in `declare export var ...`
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 2,
                      character: 0,
                    },
                    end: {
                      line: 2,
                      character: 30,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 0,
                        character: 0,
                      },
                      end: {
                        line: 2,
                        character: 30,
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
      test('textDocument/selectionRange on classes', [
        addFiles('class.js'),
        lspStartAndConnect(),
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/class.js',
          [{line: 6, character: 2}], // in class keyword
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 6,
                      character: 0,
                    },
                    end: {
                      line: 22,
                      character: 1,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 2,
                        character: 0,
                      },
                      end: {
                        line: 22,
                        character: 1,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 0,
                          character: 0,
                        },
                        end: {
                          line: 22,
                          character: 1,
                        },
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
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/class.js',
          [{line: 6, character: 16}], // `A` in `class ... extends A`
        )
          // TODO: include `extends `? not in the AST but could be useful
          // to delete.
          .verifyAllLSPMessagesInStep(
            [
              {
                method: 'textDocument/selectionRange',
                result: [
                  {
                    range: {
                      start: {
                        line: 6,
                        character: 16,
                      },
                      end: {
                        line: 6,
                        character: 17,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 6,
                          character: 0,
                        },
                        end: {
                          line: 22,
                          character: 1,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 2,
                            character: 0,
                          },
                          end: {
                            line: 22,
                            character: 1,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 0,
                              character: 0,
                            },
                            end: {
                              line: 22,
                              character: 1,
                            },
                          },
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
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/class.js',
          [{line: 6, character: 29}], // `I` in `class ... identifier I {`
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 6,
                      character: 29,
                    },
                    end: {
                      line: 6,
                      character: 30,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 6,
                        character: 18,
                      },
                      end: {
                        line: 6,
                        character: 30,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 6,
                          character: 0,
                        },
                        end: {
                          line: 22,
                          character: 1,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 2,
                            character: 0,
                          },
                          end: {
                            line: 22,
                            character: 1,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 0,
                              character: 0,
                            },
                            end: {
                              line: 22,
                              character: 1,
                            },
                          },
                        },
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
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/class.js',
          [{line: 7, character: 11}], // in foo void return type
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 7,
                      character: 9,
                    },
                    end: {
                      line: 7,
                      character: 13,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 7,
                        character: 7,
                      },
                      end: {
                        line: 7,
                        character: 13,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 7,
                          character: 5,
                        },
                        end: {
                          line: 7,
                          character: 13,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 7,
                            character: 2,
                          },
                          end: {
                            line: 9,
                            character: 3,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 6,
                              character: 31,
                            },
                            end: {
                              line: 22,
                              character: 1,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 6,
                                character: 0,
                              },
                              end: {
                                line: 22,
                                character: 1,
                              },
                            },
                            parent: {
                              range: {
                                start: {
                                  line: 2,
                                  character: 0,
                                },
                                end: {
                                  line: 22,
                                  character: 1,
                                },
                              },
                              parent: {
                                range: {
                                  start: {
                                    line: 0,
                                    character: 0,
                                  },
                                  end: {
                                    line: 22,
                                    character: 1,
                                  },
                                },
                              },
                            },
                          },
                        },
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
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/class.js',
          [{line: 8, character: 0}], // in foo method body
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 7,
                      character: 14,
                    },
                    end: {
                      line: 9,
                      character: 3,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 7,
                        character: 2,
                      },
                      end: {
                        line: 9,
                        character: 3,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 6,
                          character: 31,
                        },
                        end: {
                          line: 22,
                          character: 1,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 6,
                            character: 0,
                          },
                          end: {
                            line: 22,
                            character: 1,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 2,
                              character: 0,
                            },
                            end: {
                              line: 22,
                              character: 1,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 0,
                                character: 0,
                              },
                              end: {
                                line: 22,
                                character: 1,
                              },
                            },
                          },
                        },
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
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/class.js',
          [{line: 11, character: 7}], // in bar identifer
        )
          // TODO: should include signature
          .verifyAllLSPMessagesInStep(
            [
              {
                method: 'textDocument/selectionRange',
                result: [
                  {
                    range: {
                      start: {
                        line: 11,
                        character: 6,
                      },
                      end: {
                        line: 11,
                        character: 9,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 11,
                          character: 2,
                        },
                        end: {
                          line: 13,
                          character: 3,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 6,
                            character: 31,
                          },
                          end: {
                            line: 22,
                            character: 1,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 6,
                              character: 0,
                            },
                            end: {
                              line: 22,
                              character: 1,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 2,
                                character: 0,
                              },
                              end: {
                                line: 22,
                                character: 1,
                              },
                            },
                            parent: {
                              range: {
                                start: {
                                  line: 0,
                                  character: 0,
                                },
                                end: {
                                  line: 22,
                                  character: 1,
                                },
                              },
                            },
                          },
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
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/class.js',
          [{line: 15, character: 6}], // in baz's T param
        )
          // TODO: should include signature
          .verifyAllLSPMessagesInStep(
            [
              {
                method: 'textDocument/selectionRange',
                result: [
                  {
                    range: {
                      start: {
                        line: 15,
                        character: 6,
                      },
                      end: {
                        line: 15,
                        character: 7,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 15,
                          character: 5,
                        },
                        end: {
                          line: 15,
                          character: 8,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 15,
                            character: 5,
                          },
                          end: {
                            line: 15,
                            character: 16,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 15,
                              character: 2,
                            },
                            end: {
                              line: 17,
                              character: 3,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 6,
                                character: 31,
                              },
                              end: {
                                line: 22,
                                character: 1,
                              },
                            },
                            parent: {
                              range: {
                                start: {
                                  line: 6,
                                  character: 0,
                                },
                                end: {
                                  line: 22,
                                  character: 1,
                                },
                              },
                              parent: {
                                range: {
                                  start: {
                                    line: 2,
                                    character: 0,
                                  },
                                  end: {
                                    line: 22,
                                    character: 1,
                                  },
                                },
                                parent: {
                                  range: {
                                    start: {
                                      line: 0,
                                      character: 0,
                                    },
                                    end: {
                                      line: 22,
                                      character: 1,
                                    },
                                  },
                                },
                              },
                            },
                          },
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
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/class.js',
          [{line: 19, character: 4}], // in propA key
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 19,
                      character: 2,
                    },
                    end: {
                      line: 19,
                      character: 7,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 19,
                        character: 2,
                      },
                      end: {
                        line: 19,
                        character: 16,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 6,
                          character: 31,
                        },
                        end: {
                          line: 22,
                          character: 1,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 6,
                            character: 0,
                          },
                          end: {
                            line: 22,
                            character: 1,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 2,
                              character: 0,
                            },
                            end: {
                              line: 22,
                              character: 1,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 0,
                                character: 0,
                              },
                              end: {
                                line: 22,
                                character: 1,
                              },
                            },
                          },
                        },
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
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/class.js',
          [{line: 19, character: 12}], // in propA annot
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 19,
                      character: 9,
                    },
                    end: {
                      line: 19,
                      character: 15,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 19,
                        character: 7,
                      },
                      end: {
                        line: 19,
                        character: 15,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 19,
                          character: 2,
                        },
                        end: {
                          line: 19,
                          character: 16,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 6,
                            character: 31,
                          },
                          end: {
                            line: 22,
                            character: 1,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 6,
                              character: 0,
                            },
                            end: {
                              line: 22,
                              character: 1,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 2,
                                character: 0,
                              },
                              end: {
                                line: 22,
                                character: 1,
                              },
                            },
                            parent: {
                              range: {
                                start: {
                                  line: 0,
                                  character: 0,
                                },
                                end: {
                                  line: 22,
                                  character: 1,
                                },
                              },
                            },
                          },
                        },
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
        requestSelectionRange(
          '<PLACEHOLDER_PROJECT_URL>/class.js',
          [{line: 21, character: 22}], // in propB initializer
        ).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 21,
                      character: 19,
                    },
                    end: {
                      line: 21,
                      character: 24,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 21,
                        character: 2,
                      },
                      end: {
                        line: 21,
                        character: 25,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 6,
                          character: 31,
                        },
                        end: {
                          line: 22,
                          character: 1,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 6,
                            character: 0,
                          },
                          end: {
                            line: 22,
                            character: 1,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 2,
                              character: 0,
                            },
                            end: {
                              line: 22,
                              character: 1,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 0,
                                character: 0,
                              },
                              end: {
                                line: 22,
                                character: 1,
                              },
                            },
                          },
                        },
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
      test('textDocument/selectionRange on template literals', [
        addFiles('template_literal.js'),
        lspStartAndConnect(),
        requestSelectionRange('<PLACEHOLDER_PROJECT_URL>/template_literal.js', [
          {line: 4, character: 0},
        ]).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 2,
                      character: 10,
                    },
                    end: {
                      line: 6,
                      character: 1,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 2,
                        character: 6,
                      },
                      end: {
                        line: 6,
                        character: 1,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 2,
                          character: 0,
                        },
                        end: {
                          line: 6,
                          character: 2,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 2,
                            character: 0,
                          },
                          end: {
                            line: 14,
                            character: 2,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 0,
                              character: 0,
                            },
                            end: {
                              line: 14,
                              character: 2,
                            },
                          },
                        },
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
        requestSelectionRange('<PLACEHOLDER_PROJECT_URL>/template_literal.js', [
          {line: 10, character: 12},
        ]).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 10,
                      character: 10,
                    },
                    end: {
                      line: 10,
                      character: 13,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 10,
                        character: 10,
                      },
                      end: {
                        line: 14,
                        character: 1,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 10,
                          character: 6,
                        },
                        end: {
                          line: 14,
                          character: 1,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 10,
                            character: 0,
                          },
                          end: {
                            line: 14,
                            character: 2,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 2,
                              character: 0,
                            },
                            end: {
                              line: 14,
                              character: 2,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 0,
                                character: 0,
                              },
                              end: {
                                line: 14,
                                character: 2,
                              },
                            },
                          },
                        },
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
        requestSelectionRange('<PLACEHOLDER_PROJECT_URL>/template_literal.js', [
          {line: 12, character: 0},
        ]).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/selectionRange',
              result: [
                {
                  range: {
                    start: {
                      line: 10,
                      character: 13,
                    },
                    end: {
                      line: 14,
                      character: 1,
                    },
                  },
                  parent: {
                    range: {
                      start: {
                        line: 10,
                        character: 10,
                      },
                      end: {
                        line: 14,
                        character: 1,
                      },
                    },
                    parent: {
                      range: {
                        start: {
                          line: 10,
                          character: 6,
                        },
                        end: {
                          line: 14,
                          character: 1,
                        },
                      },
                      parent: {
                        range: {
                          start: {
                            line: 10,
                            character: 0,
                          },
                          end: {
                            line: 14,
                            character: 2,
                          },
                        },
                        parent: {
                          range: {
                            start: {
                              line: 2,
                              character: 0,
                            },
                            end: {
                              line: 14,
                              character: 2,
                            },
                          },
                          parent: {
                            range: {
                              start: {
                                line: 0,
                                character: 0,
                              },
                              end: {
                                line: 14,
                                character: 2,
                              },
                            },
                          },
                        },
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
    ];
  },
): Suite);
