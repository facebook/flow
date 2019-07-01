/*
 * @flow
 */


import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(({
  addCode, addFile, addFiles, removeFile, ideStart, ideNotification, flowCmd
}) => [
  test('Opening and closing ignored file', [
    ideStart({mode:'legacy'})
      .ideNotification('subscribeToDiagnostics')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      ),

    addFile('ignored.js')
      .waitAndVerifyNoIDEMessagesSinceStartOfStep(500)
      .noNewErrors()
      .because('The IDE has not opened ignored.js yet'),

    ideNotification('didOpen', 'ignored.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        50000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      )
      .because('The file is ignored'),

    ideNotification('didClose', 'ignored.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        50000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      )
      .because('Closing the file does not trigger recheck, just sends errors'),

    flowCmd(['status', '--strip-root'])
      .stdout(
        `
          No errors!

          The Flow server is currently in IDE lazy mode and is only checking 0/1 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      )
      .because('Still no errors'),
  ]).lazy('ide'),

  test('Opening and closing single file with no dependents or dependencies', [
    ideStart({mode:'legacy'})
      .ideNotification('subscribeToDiagnostics')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      ),

    addCode('var x: string = 123')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "startRecheck",
            "params": []
          },
          {
            "method": "endRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      )
      .noNewErrors()
      .because('The IDE has not opened test.js yet'),

    ideNotification('didOpen', 'test.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "startRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "extra": [
                      {
                        "message": [
                          {
                            "context": null,
                            "descr": "References:",
                            "type": "Blame",
                            "path": "",
                            "line": 0,
                            "endline": 0,
                            "start": 1,
                            "end": 0
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "test.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 3,
                                "column": 17,
                                "offset": 29
                              },
                              "end": {
                                "line": 3,
                                "column": 19,
                                "offset": 32
                              }
                            },
                            "path": "test.js",
                            "line": 3,
                            "endline": 3,
                            "start": 17,
                            "end": 19
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "test.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 3,
                                "column": 8,
                                "offset": 20
                              },
                              "end": {
                                "line": 3,
                                "column": 13,
                                "offset": 26
                              }
                            },
                            "path": "test.js",
                            "line": 3,
                            "endline": 3,
                            "start": 8,
                            "end": 13
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var x: string = 123",
                        "descr": "Cannot assign `123` to `x` because number [1] is incompatible with string [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "test.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 17,
                            "offset": 29
                          },
                          "end": {
                            "line": 3,
                            "column": 19,
                            "offset": 32
                          }
                        },
                        "path": "test.js",
                        "line": 3,
                        "endline": 3,
                        "start": 17,
                        "end": 19
                      }
                    ]
                  }
                ],
                "passed": false
              }
            ]
          },
          {
            "method": "endRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "extra": [
                      {
                        "message": [
                          {
                            "context": null,
                            "descr": "References:",
                            "type": "Blame",
                            "path": "",
                            "line": 0,
                            "endline": 0,
                            "start": 1,
                            "end": 0
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "test.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 3,
                                "column": 17,
                                "offset": 29
                              },
                              "end": {
                                "line": 3,
                                "column": 19,
                                "offset": 32
                              }
                            },
                            "path": "test.js",
                            "line": 3,
                            "endline": 3,
                            "start": 17,
                            "end": 19
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "test.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 3,
                                "column": 8,
                                "offset": 20
                              },
                              "end": {
                                "line": 3,
                                "column": 13,
                                "offset": 26
                              }
                            },
                            "path": "test.js",
                            "line": 3,
                            "endline": 3,
                            "start": 8,
                            "end": 13
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var x: string = 123",
                        "descr": "Cannot assign `123` to `x` because number [1] is incompatible with string [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "test.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 17,
                            "offset": 29
                          },
                          "end": {
                            "line": 3,
                            "column": 19,
                            "offset": 32
                          }
                        },
                        "path": "test.js",
                        "line": 3,
                        "endline": 3,
                        "start": 17,
                        "end": 19
                      }
                    ]
                  }
                ],
                "passed": false
              }
            ]
          }
        ],
      )
      .because('Opening the file triggers a recheck which sees the error'),

    ideNotification('didClose', 'test.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "extra": [
                      {
                        "message": [
                          {
                            "context": null,
                            "descr": "References:",
                            "type": "Blame",
                            "path": "",
                            "line": 0,
                            "endline": 0,
                            "start": 1,
                            "end": 0
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "test.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 3,
                                "column": 17,
                                "offset": 29
                              },
                              "end": {
                                "line": 3,
                                "column": 19,
                                "offset": 32
                              }
                            },
                            "path": "test.js",
                            "line": 3,
                            "endline": 3,
                            "start": 17,
                            "end": 19
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "test.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 3,
                                "column": 8,
                                "offset": 20
                              },
                              "end": {
                                "line": 3,
                                "column": 13,
                                "offset": 26
                              }
                            },
                            "path": "test.js",
                            "line": 3,
                            "endline": 3,
                            "start": 8,
                            "end": 13
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var x: string = 123",
                        "descr": "Cannot assign `123` to `x` because number [1] is incompatible with string [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "test.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 17,
                            "offset": 29
                          },
                          "end": {
                            "line": 3,
                            "column": 19,
                            "offset": 32
                          }
                        },
                        "path": "test.js",
                        "line": 3,
                        "endline": 3,
                        "start": 17,
                        "end": 19
                      }
                    ]
                  }
                ],
                "passed": false
              }
            ]
          }
        ],
      )
      .because('Closing the file does not trigger recheck, just send errors'),

    flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error ----------------------------------------------------------------------------------------------------- test.js:3:17

          Cannot assign \`123\` to \`x\` because number [1] is incompatible with string [2].

             test.js:3:17
             3| var x: string = 123
                                ^^^ [1]

          References:
             test.js:3:8
             3| var x: string = 123
                       ^^^^^^ [2]



          Found 1 error

          The Flow server is currently in IDE lazy mode and is only checking 1/1 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      )
      .because('Even though test.js is closed, it is still focused'),

    addCode('var anotherError: bool = 123')
      .newErrors(
        `
          test.js:5
            5: var anotherError: bool = 123
                                        ^^^ Cannot assign \`123\` to \`anotherError\` because number [1] is incompatible with boolean [2].
            References:
              5: var anotherError: bool = 123
                                          ^^^ [1]
              5: var anotherError: bool = 123
                                   ^^^^ [2]
        `,
      )
      .because('Changes are still noticed'),
  ]).lazy('ide'),

  // Somewhat similar to tests/quick-start-add-dependency
  test('New dependent', [
    ideStart({mode:'legacy'})
      .ideNotification('subscribeToDiagnostics')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      ),
    addFiles('focused.js', 'dependency.js', 'otherDependent.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "startRecheck",
            "params": []
          },
          {
            "method": "endRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      )
      .because('Nothing is open, so we just get the recheck start and end'),
    ideNotification('didOpen', 'focused.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "startRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "extra": [
                      {
                        "message": [
                          {
                            "context": null,
                            "descr": "References:",
                            "type": "Blame",
                            "path": "",
                            "line": 0,
                            "endline": 0,
                            "start": 1,
                            "end": 0
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "export default 123;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "dependency.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 1,
                                "column": 16,
                                "offset": 15
                              },
                              "end": {
                                "line": 1,
                                "column": 18,
                                "offset": 18
                              }
                            },
                            "path": "dependency.js",
                            "line": 1,
                            "endline": 1,
                            "start": 16,
                            "end": 18
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var focusedError: string = dependency;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "focused.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 3,
                                "column": 19,
                                "offset": 58
                              },
                              "end": {
                                "line": 3,
                                "column": 24,
                                "offset": 64
                              }
                            },
                            "path": "focused.js",
                            "line": 3,
                            "endline": 3,
                            "start": 19,
                            "end": 24
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var focusedError: string = dependency;",
                        "descr": "Cannot assign `dependency` to `focusedError` because number [1] is incompatible with string [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "focused.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 28,
                            "offset": 67
                          },
                          "end": {
                            "line": 3,
                            "column": 37,
                            "offset": 77
                          }
                        },
                        "path": "focused.js",
                        "line": 3,
                        "endline": 3,
                        "start": 28,
                        "end": 37
                      }
                    ]
                  }
                ],
                "passed": false
              }
            ]
          },
          {
            "method": "endRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "extra": [
                      {
                        "message": [
                          {
                            "context": null,
                            "descr": "References:",
                            "type": "Blame",
                            "path": "",
                            "line": 0,
                            "endline": 0,
                            "start": 1,
                            "end": 0
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "export default 123;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "dependency.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 1,
                                "column": 16,
                                "offset": 15
                              },
                              "end": {
                                "line": 1,
                                "column": 18,
                                "offset": 18
                              }
                            },
                            "path": "dependency.js",
                            "line": 1,
                            "endline": 1,
                            "start": 16,
                            "end": 18
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var focusedError: string = dependency;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "focused.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 3,
                                "column": 19,
                                "offset": 58
                              },
                              "end": {
                                "line": 3,
                                "column": 24,
                                "offset": 64
                              }
                            },
                            "path": "focused.js",
                            "line": 3,
                            "endline": 3,
                            "start": 19,
                            "end": 24
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var focusedError: string = dependency;",
                        "descr": "Cannot assign `dependency` to `focusedError` because number [1] is incompatible with string [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "focused.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 28,
                            "offset": 67
                          },
                          "end": {
                            "line": 3,
                            "column": 37,
                            "offset": 77
                          }
                        },
                        "path": "focused.js",
                        "line": 3,
                        "endline": 3,
                        "start": 28,
                        "end": 37
                      }
                    ]
                  }
                ],
                "passed": false
              }
            ]
          }
        ],
      )
      .because(
        'Opening focused.js will cause a recheck and show the error in focused.js but not (fake) dependency.js'
      ),
    flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error -------------------------------------------------------------------------------------------------- focused.js:3:28

          Cannot assign \`dependency\` to \`focusedError\` because number [1] is incompatible with string [2].

             focused.js:3:28
             3| var focusedError: string = dependency;
                                           ^^^^^^^^^^

          References:
             dependency.js:1:16
             1| export default 123;
                               ^^^ [1]
             focused.js:3:19
             3| var focusedError: string = dependency;
                                  ^^^^^^ [2]



          Found 1 error

          The Flow server is currently in IDE lazy mode and is only checking 2/4 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      ).because('There is one error now'),

    addFile('dependent.js')
      .noNewErrors()
      .because('Adding a new (fake) dependent will not recheck that dependent'),

  ]).lazy('ide'),

  // Based on tests/quick-start
  test('@flow and @noflow pragmas', [
    ideStart({mode:'legacy'})
      .ideNotification('subscribeToDiagnostics')
      .ideNotification('didOpen', 'errors.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          },
          {
            "method": "startRecheck",
            "params": []
          },
          {
            "method": "endRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      ),
    addFile('errorsWithFlowPragma.js', 'errors.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "startRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "extra": [
                      {
                        "message": [
                          {
                            "context": null,
                            "descr": "References:",
                            "type": "Blame",
                            "path": "",
                            "line": 0,
                            "endline": 0,
                            "start": 1,
                            "end": 0
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "errors.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 2,
                                "column": 17,
                                "offset": 25
                              },
                              "end": {
                                "line": 2,
                                "column": 19,
                                "offset": 28
                              }
                            },
                            "path": "errors.js",
                            "line": 2,
                            "endline": 2,
                            "start": 17,
                            "end": 19
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "errors.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 2,
                                "column": 8,
                                "offset": 16
                              },
                              "end": {
                                "line": 2,
                                "column": 13,
                                "offset": 22
                              }
                            },
                            "path": "errors.js",
                            "line": 2,
                            "endline": 2,
                            "start": 8,
                            "end": 13
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var x: string = 123;",
                        "descr": "Cannot assign `123` to `x` because number [1] is incompatible with string [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "errors.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 2,
                            "column": 17,
                            "offset": 25
                          },
                          "end": {
                            "line": 2,
                            "column": 19,
                            "offset": 28
                          }
                        },
                        "path": "errors.js",
                        "line": 2,
                        "endline": 2,
                        "start": 17,
                        "end": 19
                      }
                    ]
                  }
                ],
                "passed": false
              }
            ]
          },
          {
            "method": "endRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "extra": [
                      {
                        "message": [
                          {
                            "context": null,
                            "descr": "References:",
                            "type": "Blame",
                            "path": "",
                            "line": 0,
                            "endline": 0,
                            "start": 1,
                            "end": 0
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "errors.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 2,
                                "column": 17,
                                "offset": 25
                              },
                              "end": {
                                "line": 2,
                                "column": 19,
                                "offset": 28
                              }
                            },
                            "path": "errors.js",
                            "line": 2,
                            "endline": 2,
                            "start": 17,
                            "end": 19
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "errors.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 2,
                                "column": 8,
                                "offset": 16
                              },
                              "end": {
                                "line": 2,
                                "column": 13,
                                "offset": 22
                              }
                            },
                            "path": "errors.js",
                            "line": 2,
                            "endline": 2,
                            "start": 8,
                            "end": 13
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var x: string = 123;",
                        "descr": "Cannot assign `123` to `x` because number [1] is incompatible with string [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "errors.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 2,
                            "column": 17,
                            "offset": 25
                          },
                          "end": {
                            "line": 2,
                            "column": 19,
                            "offset": 28
                          }
                        },
                        "path": "errors.js",
                        "line": 2,
                        "endline": 2,
                        "start": 17,
                        "end": 19
                      }
                    ]
                  }
                ],
                "passed": false
              }
            ]
          }
        ],
      )
      .newErrors(
        `
          errors.js:2
            2: var x: string = 123;
                               ^^^ Cannot assign \`123\` to \`x\` because number [1] is incompatible with string [2].
            References:
              2: var x: string = 123;
                                 ^^^ [1]
              2: var x: string = 123;
                        ^^^^^^ [2]
        `,
      )
      .because('File is open and has @flow so we should get the error'),
    addFile('errorsWithNoFlowPragma.js', 'errors.js')
      .flowCmd(['status', '--strip-root'])
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "startRecheck",
            "params": []
          },
          {
            "method": "endRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      )
      .stdout(
        `
          No errors!

          The Flow server is currently in IDE lazy mode and is only checking 0/1 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      )
      .because('File is open but has @noflow so we should no errors'),
    addFile('errorsWithFlowPragma.js', 'errors.js')
      .newErrors(
        `
          errors.js:2
            2: var x: string = 123;
                               ^^^ Cannot assign \`123\` to \`x\` because number [1] is incompatible with string [2].
            References:
              2: var x: string = 123;
                                 ^^^ [1]
              2: var x: string = 123;
                        ^^^^^^ [2]
        `,
      )
      .because('Reverting back to @flow should show the error again'),
  ]).lazy('ide').flowConfig('_flowconfig_all_false'),

  // Based on tests/quick-start-add-dependency-on-cycle
  test('Open file (fake) cyclic dependency', [
    ideStart({mode:'legacy'})
      .ideNotification('subscribeToDiagnostics')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      ),
    addFiles('cycleA.js', 'cycleB.js', 'focusedWithCyclicDependency.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "startRecheck",
            "params": []
          },
          {
            "method": "endRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      ),
    ideNotification('didOpen', 'focusedWithCyclicDependency.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "startRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "extra": [
                      {
                        "message": [
                          {
                            "context": null,
                            "descr": "References:",
                            "type": "Blame",
                            "path": "",
                            "line": 0,
                            "endline": 0,
                            "start": 1,
                            "end": 0
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "focusedWithCyclicDependency.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 3,
                                "column": 17,
                                "offset": 38
                              },
                              "end": {
                                "line": 3,
                                "column": 19,
                                "offset": 41
                              }
                            },
                            "path": "focusedWithCyclicDependency.js",
                            "line": 3,
                            "endline": 3,
                            "start": 17,
                            "end": 19
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "focusedWithCyclicDependency.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 3,
                                "column": 8,
                                "offset": 29
                              },
                              "end": {
                                "line": 3,
                                "column": 13,
                                "offset": 35
                              }
                            },
                            "path": "focusedWithCyclicDependency.js",
                            "line": 3,
                            "endline": 3,
                            "start": 8,
                            "end": 13
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var x: string = 123;",
                        "descr": "Cannot assign `123` to `x` because number [1] is incompatible with string [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "focusedWithCyclicDependency.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 17,
                            "offset": 38
                          },
                          "end": {
                            "line": 3,
                            "column": 19,
                            "offset": 41
                          }
                        },
                        "path": "focusedWithCyclicDependency.js",
                        "line": 3,
                        "endline": 3,
                        "start": 17,
                        "end": 19
                      }
                    ]
                  }
                ],
                "passed": false
              }
            ]
          },
          {
            "method": "endRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "extra": [
                      {
                        "message": [
                          {
                            "context": null,
                            "descr": "References:",
                            "type": "Blame",
                            "path": "",
                            "line": 0,
                            "endline": 0,
                            "start": 1,
                            "end": 0
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "focusedWithCyclicDependency.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 3,
                                "column": 17,
                                "offset": 38
                              },
                              "end": {
                                "line": 3,
                                "column": 19,
                                "offset": 41
                              }
                            },
                            "path": "focusedWithCyclicDependency.js",
                            "line": 3,
                            "endline": 3,
                            "start": 17,
                            "end": 19
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: string = 123;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "focusedWithCyclicDependency.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 3,
                                "column": 8,
                                "offset": 29
                              },
                              "end": {
                                "line": 3,
                                "column": 13,
                                "offset": 35
                              }
                            },
                            "path": "focusedWithCyclicDependency.js",
                            "line": 3,
                            "endline": 3,
                            "start": 8,
                            "end": 13
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var x: string = 123;",
                        "descr": "Cannot assign `123` to `x` because number [1] is incompatible with string [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "focusedWithCyclicDependency.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 17,
                            "offset": 38
                          },
                          "end": {
                            "line": 3,
                            "column": 19,
                            "offset": 41
                          }
                        },
                        "path": "focusedWithCyclicDependency.js",
                        "line": 3,
                        "endline": 3,
                        "start": 17,
                        "end": 19
                      }
                    ]
                  }
                ],
                "passed": false
              }
            ]
          }
        ],
      ),
      flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error ------------------------------------------------------------------------------ focusedWithCyclicDependency.js:3:17

          Cannot assign \`123\` to \`x\` because number [1] is incompatible with string [2].

             focusedWithCyclicDependency.js:3:17
             3| var x: string = 123;
                                ^^^ [1]

          References:
             focusedWithCyclicDependency.js:3:8
             3| var x: string = 123;
                       ^^^^^^ [2]



          Found 1 error

          The Flow server is currently in IDE lazy mode and is only checking 3/4 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      )
      .because('Nothing should blow up and we should see the single error'),
  ]).lazy('ide'),

  // Based on tests/quick-start-check-contents
  test('check-contents should pull in dependency', [
    ideStart({mode:'legacy'})
      .ideNotification('subscribeToDiagnostics')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      ),
    addFiles('focused.js', 'dependency.js', 'otherDependent.js')
      .flowCmd(['status', '--strip-root'])
      .stdout(
        `
          No errors!

          The Flow server is currently in IDE lazy mode and is only checking 0/4 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      )
      .because('No files are open yet'),
    flowCmd(['check-contents', 'focused.js'], 'focused.js')
      .stdout(
        `
          Error -------------------------------------------------------------------------------------------------- focused.js:3:28

          Cannot assign \`dependency\` to \`focusedError\` because number [1] is incompatible with string [2].

             focused.js:3:28
             3| var focusedError: string = dependency;
                                           ^^^^^^^^^^

          References:
             dependency.js:1:16
             1| export default 123;
                               ^^^ [1]
             focused.js:3:19
             3| var focusedError: string = dependency;
                                  ^^^^^^ [2]



          Found 1 error

        `,
      )
      .because('check-contents will report the error in the file it checks'),
    flowCmd(['status', '--strip-root'])
      .stdout(
        `
          No errors!

          The Flow server is currently in IDE lazy mode and is only checking 1/4 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      )
      .because(
        'As a side effect of check-contents, dependency.js is added to the checkset. At the moment we do not prune the checked set.'
      ),
  ]).lazy('ide'),

  // Based on tests/quick-start-delete-dependency
  test('Delete dependency', [
    ideStart({mode:'legacy'})
      .ideNotification('subscribeToDiagnostics')
      .ideNotification('didOpen', 'focused.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          },
          {
            "method": "startRecheck",
            "params": []
          },
          {
            "method": "endRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      ),
    addFiles('focused.js', 'dependency.js', 'otherDependent.js')
      .flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error -------------------------------------------------------------------------------------------------- focused.js:3:28

          Cannot assign \`dependency\` to \`focusedError\` because number [1] is incompatible with string [2].

             focused.js:3:28
             3| var focusedError: string = dependency;
                                           ^^^^^^^^^^

          References:
             dependency.js:1:16
             1| export default 123;
                               ^^^ [1]
             focused.js:3:19
             3| var focusedError: string = dependency;
                                  ^^^^^^ [2]



          Found 1 error

          The Flow server is currently in IDE lazy mode and is only checking 2/4 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      )
      .because('One error: for the focused file, not the (fake) dependency'),
    removeFile('dependency.js')
      .flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error -------------------------------------------------------------------------------------------------- focused.js:1:24

          Cannot resolve module \`./dependency\`.

             1| import dependency from './dependency';
                                       ^^^^^^^^^^^^^^



          Found 1 error

          The Flow server is currently in IDE lazy mode and is only checking 1/3 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      )
      .because('We get missing module error'),
    addFile('dependency.js')
    .flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error -------------------------------------------------------------------------------------------------- focused.js:3:28

          Cannot assign \`dependency\` to \`focusedError\` because number [1] is incompatible with string [2].

             focused.js:3:28
             3| var focusedError: string = dependency;
                                           ^^^^^^^^^^

          References:
             dependency.js:1:16
             1| export default 123;
                               ^^^ [1]
             focused.js:3:19
             3| var focusedError: string = dependency;
                                  ^^^^^^ [2]



          Found 1 error

          The Flow server is currently in IDE lazy mode and is only checking 2/4 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      )
      .because('Restoring the file takes us back to the original one error'),
  ]).lazy('ide'),

  test('Open a dependency', [
    ideStart({mode:'legacy'})
      .ideNotification('subscribeToDiagnostics')
      .ideNotification('didOpen', 'focused.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          },
          {
            "method": "startRecheck",
            "params": []
          },
          {
            "method": "endRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      ),
    addFiles('focused.js', 'dependency.js', 'otherDependent.js')
      .newErrors(
        `
          focused.js:3
            3: var focusedError: string = dependency;
                                          ^^^^^^^^^^ Cannot assign \`dependency\` to \`focusedError\` because number [1] is incompatible with string [2].
            References:
              1: export default 123;
                                ^^^ [1]. See: dependency.js:1
              3: var focusedError: string = dependency;
                                   ^^^^^^ [2]
        `,
      )
      .because('Other dependent is a dependent of a dependency, so is not checked'),
    ideNotification('didOpen', 'dependency.js')
      // Unfortunately ideMessagesSinceStartOfStep doesn't work here since the
      // order of the streamed errors isn't fixed :(
      .sleep(500),
    flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error ----------------------------------------------------------------------------------------------- dependency.js:3:31

          Cannot assign \`123\` to \`dependencyError\` because number [1] is incompatible with string [2].

             dependency.js:3:31
             3| var dependencyError: string = 123;
                                              ^^^ [1]

          References:
             dependency.js:3:22
             3| var dependencyError: string = 123;
                                     ^^^^^^ [2]


          Error -------------------------------------------------------------------------------------------------- focused.js:3:28

          Cannot assign \`dependency\` to \`focusedError\` because number [1] is incompatible with string [2].

             focused.js:3:28
             3| var focusedError: string = dependency;
                                           ^^^^^^^^^^

          References:
             dependency.js:1:16
             1| export default 123;
                               ^^^ [1]
             focused.js:3:19
             3| var focusedError: string = dependency;
                                  ^^^^^^ [2]


          Error ------------------------------------------------------------------------------------------- otherDependent.js:3:35

          Cannot assign \`dependency\` to \`otherDependentError\` because number [1] is incompatible with string [2].

             otherDependent.js:3:35
             3| var otherDependentError: string = dependency;
                                                  ^^^^^^^^^^

          References:
             dependency.js:1:16
             1| export default 123;
                               ^^^ [1]
             otherDependent.js:3:26
             3| var otherDependentError: string = dependency;
                                         ^^^^^^ [2]



          Found 3 errors

          The Flow server is currently in IDE lazy mode and is only checking 3/4 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      )
      .because('Focusing on a dependency now checks its dependents'),
  ]).lazy('ide'),

  test('Remove and restore an open file', [
    ideStart({mode:'legacy'})
      .ideNotification('subscribeToDiagnostics')
      .ideNotification('didOpen', 'focused.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          },
          {
            "method": "startRecheck",
            "params": []
          },
          {
            "method": "endRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      ),
    addFiles('focused.js', 'dependency.js', 'otherDependent.js')
      .flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error -------------------------------------------------------------------------------------------------- focused.js:3:28

          Cannot assign \`dependency\` to \`focusedError\` because number [1] is incompatible with string [2].

             focused.js:3:28
             3| var focusedError: string = dependency;
                                           ^^^^^^^^^^

          References:
             dependency.js:1:16
             1| export default 123;
                               ^^^ [1]
             focused.js:3:19
             3| var focusedError: string = dependency;
                                  ^^^^^^ [2]



          Found 1 error

          The Flow server is currently in IDE lazy mode and is only checking 2/4 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      ),
    removeFile('focused.js')
      .addFile('focused.js')
      .flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error -------------------------------------------------------------------------------------------------- focused.js:3:28

          Cannot assign \`dependency\` to \`focusedError\` because number [1] is incompatible with string [2].

             focused.js:3:28
             3| var focusedError: string = dependency;
                                           ^^^^^^^^^^

          References:
             dependency.js:1:16
             1| export default 123;
                               ^^^ [1]
             focused.js:3:19
             3| var focusedError: string = dependency;
                                  ^^^^^^ [2]



          Found 1 error

          The Flow server is currently in IDE lazy mode and is only checking 2/4 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      )
      .because('We should be back at our starting state'),
  ]).lazy('ide'),

  test('flow force-recheck --focus', [
    ideStart({mode:'legacy'})
      .ideNotification('subscribeToDiagnostics')
      .ideNotification('didOpen', 'focused.js')
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        10000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          },
          {
            "method": "startRecheck",
            "params": []
          },
          {
            "method": "endRecheck",
            "params": []
          },
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "jsonVersion": "1",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      ),
    addFiles('focused.js', 'dependency.js', 'otherDependent.js')
      .flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error -------------------------------------------------------------------------------------------------- focused.js:3:28

          Cannot assign \`dependency\` to \`focusedError\` because number [1] is incompatible with string [2].

             focused.js:3:28
             3| var focusedError: string = dependency;
                                           ^^^^^^^^^^

          References:
             dependency.js:1:16
             1| export default 123;
                               ^^^ [1]
             focused.js:3:19
             3| var focusedError: string = dependency;
                                  ^^^^^^ [2]



          Found 1 error

          The Flow server is currently in IDE lazy mode and is only checking 2/4 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      ).because("otherDependent's errors are ignored due to lazy mode"),
    flowCmd(['force-recheck', '--focus', 'dependency.js'])
      .flowCmd(['status', '--strip-root'])
      .stdout(
        `

          Error ----------------------------------------------------------------------------------------------- dependency.js:3:31

          Cannot assign \`123\` to \`dependencyError\` because number [1] is incompatible with string [2].

             dependency.js:3:31
             3| var dependencyError: string = 123;
                                              ^^^ [1]

          References:
             dependency.js:3:22
             3| var dependencyError: string = 123;
                                     ^^^^^^ [2]


          Error -------------------------------------------------------------------------------------------------- focused.js:3:28

          Cannot assign \`dependency\` to \`focusedError\` because number [1] is incompatible with string [2].

             focused.js:3:28
             3| var focusedError: string = dependency;
                                           ^^^^^^^^^^

          References:
             dependency.js:1:16
             1| export default 123;
                               ^^^ [1]
             focused.js:3:19
             3| var focusedError: string = dependency;
                                  ^^^^^^ [2]


          Error ------------------------------------------------------------------------------------------- otherDependent.js:3:35

          Cannot assign \`dependency\` to \`otherDependentError\` because number [1] is incompatible with string [2].

             otherDependent.js:3:35
             3| var otherDependentError: string = dependency;
                                                  ^^^^^^^^^^

          References:
             dependency.js:1:16
             1| export default 123;
                               ^^^ [1]
             otherDependent.js:3:26
             3| var otherDependentError: string = dependency;
                                         ^^^^^^ [2]



          Found 3 errors

          The Flow server is currently in IDE lazy mode and is only checking 3/4 files.
          To learn more, visit flow.org/en/docs/lang/lazy-modes

        `,
      ).because('force-recheck --focus promotes dependency to focused, so we see the error in otherDependent'),
  ]).lazy('ide'),
]);
