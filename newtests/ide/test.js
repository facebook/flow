/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */


import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(({ideStart, ideNotification, ideRequest, addCode, addFile}) => [
  test('The initial subscribe does not send existing errors', [
    addCode('var x: string = 123;')
      .newErrors(
        `
          test.js:3
            3: var x: string = 123;
                               ^^^ Cannot assign \`123\` to \`x\` because number [1] is incompatible with string [2].
            References:
              3: var x: string = 123;
                                 ^^^ [1]
              3: var x: string = 123;
                        ^^^^^^ [2]
        `,
      ),
    ideStart({mode: 'legacy'})
      .waitAndVerifyNoIDEMessagesSinceStartOfStep(500)
      .because('We are connected, but not subscribed'),
    ideNotification('subscribeToDiagnostics')
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
                            "context": "var x: string = 123;",
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
                            "context": "var x: string = 123;",
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
                        "context": "var x: string = 123;",
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
      .because('Subscribing sends all the existing errors'),
  ]),

  test('Recheck behavior', [
    addFile('existingError.js')
      .ideStart({mode:'legacy'})
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
                            "context": "var existingError: number = true;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "existingError.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 1,
                                "column": 29,
                                "offset": 28
                              },
                              "end": {
                                "line": 1,
                                "column": 32,
                                "offset": 32
                              }
                            },
                            "path": "existingError.js",
                            "line": 1,
                            "endline": 1,
                            "start": 29,
                            "end": 32
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var existingError: number = true;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "existingError.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 1,
                                "column": 20,
                                "offset": 19
                              },
                              "end": {
                                "line": 1,
                                "column": 25,
                                "offset": 25
                              }
                            },
                            "path": "existingError.js",
                            "line": 1,
                            "endline": 1,
                            "start": 20,
                            "end": 25
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var existingError: number = true;",
                        "descr": "Cannot assign `true` to `existingError` because boolean [1] is incompatible with number [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "existingError.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 1,
                            "column": 29,
                            "offset": 28
                          },
                          "end": {
                            "line": 1,
                            "column": 32,
                            "offset": 32
                          }
                        },
                        "path": "existingError.js",
                        "line": 1,
                        "endline": 1,
                        "start": 29,
                        "end": 32
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
          existingError.js:1
            1: var existingError: number = true;
                                           ^^^^ Cannot assign \`true\` to \`existingError\` because boolean [1] is incompatible with number [2].
            References:
              1: var existingError: number = true;
                                             ^^^^ [1]
              1: var existingError: number = true;
                                    ^^^^^^ [2]
        `,
      ),
    addCode('var notAnError: number = 123;')
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
                            "context": "var existingError: number = true;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "existingError.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 1,
                                "column": 29,
                                "offset": 28
                              },
                              "end": {
                                "line": 1,
                                "column": 32,
                                "offset": 32
                              }
                            },
                            "path": "existingError.js",
                            "line": 1,
                            "endline": 1,
                            "start": 29,
                            "end": 32
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var existingError: number = true;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "existingError.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 1,
                                "column": 20,
                                "offset": 19
                              },
                              "end": {
                                "line": 1,
                                "column": 25,
                                "offset": 25
                              }
                            },
                            "path": "existingError.js",
                            "line": 1,
                            "endline": 1,
                            "start": 20,
                            "end": 25
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var existingError: number = true;",
                        "descr": "Cannot assign `true` to `existingError` because boolean [1] is incompatible with number [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "existingError.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 1,
                            "column": 29,
                            "offset": 28
                          },
                          "end": {
                            "line": 1,
                            "column": 32,
                            "offset": 32
                          }
                        },
                        "path": "existingError.js",
                        "line": 1,
                        "endline": 1,
                        "start": 29,
                        "end": 32
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
      .because('No errors should be streamed during the recheck'),
    addCode('var newError: string = 123;')
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
                            "context": "var newError: string = 123;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "test.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 5,
                                "column": 24,
                                "offset": 67
                              },
                              "end": {
                                "line": 5,
                                "column": 26,
                                "offset": 70
                              }
                            },
                            "path": "test.js",
                            "line": 5,
                            "endline": 5,
                            "start": 24,
                            "end": 26
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var newError: string = 123;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "test.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 5,
                                "column": 15,
                                "offset": 58
                              },
                              "end": {
                                "line": 5,
                                "column": 20,
                                "offset": 64
                              }
                            },
                            "path": "test.js",
                            "line": 5,
                            "endline": 5,
                            "start": 15,
                            "end": 20
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var newError: string = 123;",
                        "descr": "Cannot assign `123` to `newError` because number [1] is incompatible with string [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "test.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 5,
                            "column": 24,
                            "offset": 67
                          },
                          "end": {
                            "line": 5,
                            "column": 26,
                            "offset": 70
                          }
                        },
                        "path": "test.js",
                        "line": 5,
                        "endline": 5,
                        "start": 24,
                        "end": 26
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
                            "context": "var existingError: number = true;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "existingError.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 1,
                                "column": 29,
                                "offset": 28
                              },
                              "end": {
                                "line": 1,
                                "column": 32,
                                "offset": 32
                              }
                            },
                            "path": "existingError.js",
                            "line": 1,
                            "endline": 1,
                            "start": 29,
                            "end": 32
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var existingError: number = true;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "existingError.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 1,
                                "column": 20,
                                "offset": 19
                              },
                              "end": {
                                "line": 1,
                                "column": 25,
                                "offset": 25
                              }
                            },
                            "path": "existingError.js",
                            "line": 1,
                            "endline": 1,
                            "start": 20,
                            "end": 25
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var existingError: number = true;",
                        "descr": "Cannot assign `true` to `existingError` because boolean [1] is incompatible with number [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "existingError.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 1,
                            "column": 29,
                            "offset": 28
                          },
                          "end": {
                            "line": 1,
                            "column": 32,
                            "offset": 32
                          }
                        },
                        "path": "existingError.js",
                        "line": 1,
                        "endline": 1,
                        "start": 29,
                        "end": 32
                      }
                    ]
                  },
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
                            "context": "var newError: string = 123;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "test.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 5,
                                "column": 24,
                                "offset": 67
                              },
                              "end": {
                                "line": 5,
                                "column": 26,
                                "offset": 70
                              }
                            },
                            "path": "test.js",
                            "line": 5,
                            "endline": 5,
                            "start": 24,
                            "end": 26
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var newError: string = 123;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "test.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 5,
                                "column": 15,
                                "offset": 58
                              },
                              "end": {
                                "line": 5,
                                "column": 20,
                                "offset": 64
                              }
                            },
                            "path": "test.js",
                            "line": 5,
                            "endline": 5,
                            "start": 15,
                            "end": 20
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "var newError: string = 123;",
                        "descr": "Cannot assign `123` to `newError` because number [1] is incompatible with string [2].",
                        "type": "Blame",
                        "loc": {
                          "source": "test.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 5,
                            "column": 24,
                            "offset": 67
                          },
                          "end": {
                            "line": 5,
                            "column": 26,
                            "offset": 70
                          }
                        },
                        "path": "test.js",
                        "line": 5,
                        "endline": 5,
                        "start": 24,
                        "end": 26
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
        'We get startRecheck when recheck starts and endRecheck when it ends.'
        + ' During recheck, we get incremental results.'
        + ' After recheck, we get total results.'
      ),
  ]),

  test('autocomplete', [
    ideStart({mode:'legacy'})
      .ideRequestAndWaitUntilResponse('autocomplete', 'test.js', 1, 12, "({x: 123}).;")
      .waitAndVerifyAllIDEMessagesContentSinceStartOfStep(
        0, // no need for timeout here since we already waited for the response
        [
          {
            "method": "autocomplete",
            "result": {
              "result": [
                {
                  "name": "x",
                  "type": "number",
                  "func_details": null,
                  "path": "test.js",
                  "line": 1,
                  "endline": 1,
                  "start": 6,
                  "end": 8
                }
              ]
            }
          }
        ],
      ),
  ]),

  /* This is a regression test for a bug where we would drop new connections
   * that appeared during a recheck */
  test('connect during recheck', [
    // For some reason this order of actions triggered the bug
    ideStart({mode:'legacy'})
      .addCode('var x = 123')
      .waitAndVerifyNoIDEMessagesSinceStartOfStep(100)
      .because('Starting the IDE does not fire any messages'),
    ideNotification('subscribeToDiagnostics')
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
      )
      .because('There are no errors'),
  ]),


  test('didOpen before subscribe', [
    addFile('fileWithWarning.js'),
    ideStart({mode:'legacy'})
      .ideNotification('didOpen', 'fileWithWarning.js')
      .waitAndVerifyNoIDEMessagesSinceStartOfStep(500)
      .because('We have not subscribed yet, so there is no response on open'),

    ideNotification('subscribeToDiagnostics')
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
                    "kind": "lint",
                    "level": "warning",
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
                            "context": "var x: ?boolean = true;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "fileWithWarning.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 2,
                                "column": 9,
                                "offset": 38
                              },
                              "end": {
                                "line": 2,
                                "column": 15,
                                "offset": 45
                              }
                            },
                            "path": "fileWithWarning.js",
                            "line": 2,
                            "endline": 2,
                            "start": 9,
                            "end": 15
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: ?boolean = true;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "fileWithWarning.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 2,
                                "column": 8,
                                "offset": 37
                              },
                              "end": {
                                "line": 2,
                                "column": 15,
                                "offset": 45
                              }
                            },
                            "path": "fileWithWarning.js",
                            "line": 2,
                            "endline": 2,
                            "start": 8,
                            "end": 15
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "if (x) {",
                        "descr": "Sketchy null check on boolean [1] which is potentially false. Perhaps you meant to check for null or undefined [2]? (`sketchy-null-bool`)",
                        "type": "Blame",
                        "loc": {
                          "source": "fileWithWarning.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 5,
                            "offset": 58
                          },
                          "end": {
                            "line": 3,
                            "column": 5,
                            "offset": 59
                          }
                        },
                        "path": "fileWithWarning.js",
                        "line": 3,
                        "endline": 3,
                        "start": 5,
                        "end": 5
                      }
                    ]
                  }
                ],
                "passed": true
              }
            ]
          }
        ],
      )
      .because('We report warnings on subscribe for the open file'),
  ]),

  test('didOpen after subscribe', [
    addFile('fileWithWarning.js'),
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
      )
      .because('We do not report warnings in files that are not open'),

    ideNotification('didOpen', 'fileWithWarning.js')
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
                    "kind": "lint",
                    "level": "warning",
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
                            "context": "var x: ?boolean = true;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "fileWithWarning.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 2,
                                "column": 9,
                                "offset": 38
                              },
                              "end": {
                                "line": 2,
                                "column": 15,
                                "offset": 45
                              }
                            },
                            "path": "fileWithWarning.js",
                            "line": 2,
                            "endline": 2,
                            "start": 9,
                            "end": 15
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: ?boolean = true;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "fileWithWarning.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 2,
                                "column": 8,
                                "offset": 37
                              },
                              "end": {
                                "line": 2,
                                "column": 15,
                                "offset": 45
                              }
                            },
                            "path": "fileWithWarning.js",
                            "line": 2,
                            "endline": 2,
                            "start": 8,
                            "end": 15
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "if (x) {",
                        "descr": "Sketchy null check on boolean [1] which is potentially false. Perhaps you meant to check for null or undefined [2]? (`sketchy-null-bool`)",
                        "type": "Blame",
                        "loc": {
                          "source": "fileWithWarning.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 5,
                            "offset": 58
                          },
                          "end": {
                            "line": 3,
                            "column": 5,
                            "offset": 59
                          }
                        },
                        "path": "fileWithWarning.js",
                        "line": 3,
                        "endline": 3,
                        "start": 5,
                        "end": 5
                      }
                    ]
                  }
                ],
                "passed": true
              }
            ]
          }
        ],
      )
      .because('We should receive the warning when we open the file'),

      ideNotification('didOpen', 'fileWithWarning.js')
        .waitAndVerifyNoIDEMessagesSinceStartOfStep(500)
        .because(
          'When we open an already open file, we dont get the current errors',
        ),
  ]),

  test('didClose before subscribe', [
    addFile('fileWithWarning.js'),
    ideStart({mode:'legacy'})
      .ideNotification('didOpen', 'fileWithWarning.js')
      .ideNotification('didClose', 'fileWithWarning.js')
      .waitAndVerifyNoIDEMessagesSinceStartOfStep(500)
      .because(
        'We have not subscribed yet, so there is no response on open or close',
      ),

    ideNotification('subscribeToDiagnostics')
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
      )
      .because('We closed the file, so we dont report the warning'),
  ]),

  test('didClose after subscribe', [
    addFile('fileWithWarning.js'),
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
      )
      .because('Subscribing gives us the current errors'),

    ideNotification('didOpen', 'fileWithWarning.js')
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
                    "kind": "lint",
                    "level": "warning",
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
                            "context": "var x: ?boolean = true;",
                            "descr": "[1]",
                            "type": "Blame",
                            "loc": {
                              "source": "fileWithWarning.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 2,
                                "column": 9,
                                "offset": 38
                              },
                              "end": {
                                "line": 2,
                                "column": 15,
                                "offset": 45
                              }
                            },
                            "path": "fileWithWarning.js",
                            "line": 2,
                            "endline": 2,
                            "start": 9,
                            "end": 15
                          }
                        ]
                      },
                      {
                        "message": [
                          {
                            "context": "var x: ?boolean = true;",
                            "descr": "[2]",
                            "type": "Blame",
                            "loc": {
                              "source": "fileWithWarning.js",
                              "type": "SourceFile",
                              "start": {
                                "line": 2,
                                "column": 8,
                                "offset": 37
                              },
                              "end": {
                                "line": 2,
                                "column": 15,
                                "offset": 45
                              }
                            },
                            "path": "fileWithWarning.js",
                            "line": 2,
                            "endline": 2,
                            "start": 8,
                            "end": 15
                          }
                        ]
                      }
                    ],
                    "message": [
                      {
                        "context": "if (x) {",
                        "descr": "Sketchy null check on boolean [1] which is potentially false. Perhaps you meant to check for null or undefined [2]? (`sketchy-null-bool`)",
                        "type": "Blame",
                        "loc": {
                          "source": "fileWithWarning.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 5,
                            "offset": 58
                          },
                          "end": {
                            "line": 3,
                            "column": 5,
                            "offset": 59
                          }
                        },
                        "path": "fileWithWarning.js",
                        "line": 3,
                        "endline": 3,
                        "start": 5,
                        "end": 5
                      }
                    ]
                  }
                ],
                "passed": true
              }
            ]
          }
        ],
      )
      .because('When we open a new file we get the current errors'),

    ideNotification('didClose', 'fileWithWarning.js')
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
      )
      .because('When we close a new file we get the current errors'),

    ideNotification('didClose', 'fileWithWarning.js')
      .waitAndVerifyNoIDEMessagesSinceStartOfStep(500)
      .because(
        'When we close an already closed file, we dont get the current errors',
      ),
  ]),

  test('Stop the flow ide command without killing the server', [
    ideStart({mode: 'legacy'}),
    addCode('var x = 123')
      .ideStop()
      .sleep(500),
    addCode('var y: string = 123')
      .newErrors(
        `
          test.js:5
            5: var y: string = 123
                               ^^^ Cannot assign \`123\` to \`y\` because number [1] is incompatible with string [2].
            References:
              5: var y: string = 123
                                 ^^^ [1]
              5: var y: string = 123
                        ^^^^^^ [2]
        `,
      )
      .because('Stopping the flow ide command used to kill the server accidentally'),
  ]),
]);
