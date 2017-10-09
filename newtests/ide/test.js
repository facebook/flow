/*
 * @flow
 * @lint-ignore-every LINE_WRAP1
 */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({ideStart, ideNotification, ideRequest, addCode, addFile}) => [
  test('The initial subscribe does not send existing errors', [
    addCode('var x: string = 123;')
      .newErrors(
        `
          test.js:3
            3: var x: string = 123;
                               ^^^ number. This type is incompatible with
            3: var x: string = 123;
                      ^^^^^^ string
        `,
      ),
    ideStart()
      .ideNoNewMessagesAfterSleep(500)
      .because('We are connected, but not subscribed'),
    ideNotification('subscribeToDiagnostics')
      .ideNewMessagesWithTimeout(
        5000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "message": [
                      {
                        "context": "var x: string = 123;",
                        "descr": "number",
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
                      },
                      {
                        "context": null,
                        "descr": "This type is incompatible with",
                        "type": "Comment",
                        "path": "",
                        "line": 0,
                        "endline": 0,
                        "start": 1,
                        "end": 0
                      },
                      {
                        "context": "var x: string = 123;",
                        "descr": "string",
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
      .ideStart()
      .ideNotification('subscribeToDiagnostics')
      .ideNewMessagesWithTimeout(
        5000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "message": [
                      {
                        "context": "var existingError: number = true;",
                        "descr": "boolean",
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
                      },
                      {
                        "context": null,
                        "descr": "This type is incompatible with",
                        "type": "Comment",
                        "path": "",
                        "line": 0,
                        "endline": 0,
                        "start": 1,
                        "end": 0
                      },
                      {
                        "context": "var existingError: number = true;",
                        "descr": "number",
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
                                           ^^^^ boolean. This type is incompatible with
            1: var existingError: number = true;
                                  ^^^^^^ number
        `,
      ),
    addCode('var notAnError: number = 123;')
      .ideNewMessagesWithTimeout(
        5000,
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
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "message": [
                      {
                        "context": "var existingError: number = true;",
                        "descr": "boolean",
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
                      },
                      {
                        "context": null,
                        "descr": "This type is incompatible with",
                        "type": "Comment",
                        "path": "",
                        "line": 0,
                        "endline": 0,
                        "start": 1,
                        "end": 0
                      },
                      {
                        "context": "var existingError: number = true;",
                        "descr": "number",
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
                "passed": false
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
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "message": [
                      {
                        "context": "var existingError: number = true;",
                        "descr": "boolean",
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
                      },
                      {
                        "context": null,
                        "descr": "This type is incompatible with",
                        "type": "Comment",
                        "path": "",
                        "line": 0,
                        "endline": 0,
                        "start": 1,
                        "end": 0
                      },
                      {
                        "context": "var existingError: number = true;",
                        "descr": "number",
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
                "passed": false
              }
            ]
          }
        ],
      )
      .because('No errors should be streamed during the recheck'),
    addCode('var newError: string = 123;')
      .ideNewMessagesWithTimeout(
        5000,
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
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "message": [
                      {
                        "context": "var newError: string = 123;",
                        "descr": "number",
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
                      },
                      {
                        "context": null,
                        "descr": "This type is incompatible with",
                        "type": "Comment",
                        "path": "",
                        "line": 0,
                        "endline": 0,
                        "start": 1,
                        "end": 0
                      },
                      {
                        "context": "var newError: string = 123;",
                        "descr": "string",
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
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "message": [
                      {
                        "context": "var existingError: number = true;",
                        "descr": "boolean",
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
                      },
                      {
                        "context": null,
                        "descr": "This type is incompatible with",
                        "type": "Comment",
                        "path": "",
                        "line": 0,
                        "endline": 0,
                        "start": 1,
                        "end": 0
                      },
                      {
                        "context": "var existingError: number = true;",
                        "descr": "number",
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
                  },
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "message": [
                      {
                        "context": "var newError: string = 123;",
                        "descr": "number",
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
                      },
                      {
                        "context": null,
                        "descr": "This type is incompatible with",
                        "type": "Comment",
                        "path": "",
                        "line": 0,
                        "endline": 0,
                        "start": 1,
                        "end": 0
                      },
                      {
                        "context": "var newError: string = 123;",
                        "descr": "string",
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
                "passed": false
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
                "errors": [
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "message": [
                      {
                        "context": "var existingError: number = true;",
                        "descr": "boolean",
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
                      },
                      {
                        "context": null,
                        "descr": "This type is incompatible with",
                        "type": "Comment",
                        "path": "",
                        "line": 0,
                        "endline": 0,
                        "start": 1,
                        "end": 0
                      },
                      {
                        "context": "var existingError: number = true;",
                        "descr": "number",
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
                  },
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "message": [
                      {
                        "context": "var newError: string = 123;",
                        "descr": "number",
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
                      },
                      {
                        "context": null,
                        "descr": "This type is incompatible with",
                        "type": "Comment",
                        "path": "",
                        "line": 0,
                        "endline": 0,
                        "start": 1,
                        "end": 0
                      },
                      {
                        "context": "var newError: string = 123;",
                        "descr": "string",
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
    ideStart()
      .ideRequest('autocomplete', 'test.js', 1, 12, "({x: 123}).;")
      .ideNewMessagesWithTimeout(
        5000,
        [
          {
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
        ],
      ),
  ]),

  /* This is a regression test for a bug where we would drop new connections
   * that appeared during a recheck */
  test('connect during recheck', [
    // For some reason this order of actions triggered the bug
    ideStart()
      .addCode('var x = 123')
      .ideNoNewMessagesAfterSleep(100)
      .because('Starting the IDE does not fire any messages'),
    ideNotification('subscribeToDiagnostics')
      .ideNewMessagesWithTimeout(
        5000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
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
    ideStart()
      .ideNotification('didOpen', 'fileWithWarning.js')
      .ideNoNewMessagesAfterSleep(500)
      .because('We have not subscribed yet, so there is no response on open'),

    ideNotification('subscribeToDiagnostics')
      .ideNewMessagesWithTimeout(
        5000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "errors": [
                  {
                    "extra": [
                      {
                        "message": [
                          {
                            "context": "var x: ?boolean = true;",
                            "descr": "Potentially null/undefined value.",
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
                          },
                          {
                            "context": "var x: ?boolean = true;",
                            "descr": "Potentially false value.",
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
                      }
                    ],
                    "kind": "lint",
                    "level": "warning",
                    "suppressions": [],
                    "message": [
                      {
                        "context": "if (x) {",
                        "descr": "sketchy-null-bool: Sketchy null check on boolean value. Perhaps you meant to check for null instead of for existence?",
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
    ideStart()
      .ideNotification('subscribeToDiagnostics')
      .ideNewMessagesWithTimeout(
        5000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      )
      .because('We do not report warnings in files that are not open'),

    ideNotification('didOpen', 'fileWithWarning.js')
      .ideNewMessagesWithTimeout(
        5000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "errors": [
                  {
                    "extra": [
                      {
                        "message": [
                          {
                            "context": "var x: ?boolean = true;",
                            "descr": "Potentially null/undefined value.",
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
                          },
                          {
                            "context": "var x: ?boolean = true;",
                            "descr": "Potentially false value.",
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
                      }
                    ],
                    "kind": "lint",
                    "level": "warning",
                    "suppressions": [],
                    "message": [
                      {
                        "context": "if (x) {",
                        "descr": "sketchy-null-bool: Sketchy null check on boolean value. Perhaps you meant to check for null instead of for existence?",
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
        .ideNoNewMessagesAfterSleep(500)
        .because(
          'When we open an already open file, we dont get the current errors',
        ),
  ]),

  test('didClose before subscribe', [
    addFile('fileWithWarning.js'),
    ideStart()
      .ideNotification('didOpen', 'fileWithWarning.js')
      .ideNotification('didClose', 'fileWithWarning.js')
      .ideNoNewMessagesAfterSleep(500)
      .because(
        'We have not subscribed yet, so there is no response on open or close',
      ),

    ideNotification('subscribeToDiagnostics')
      .ideNewMessagesWithTimeout(
        5000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
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
    ideStart()
      .ideNotification('subscribeToDiagnostics')
      .ideNewMessagesWithTimeout(
        5000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      )
      .because('Subscribing gives us the current errors'),

    ideNotification('didOpen', 'fileWithWarning.js')
      .ideNewMessagesWithTimeout(
        5000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "errors": [
                  {
                    "extra": [
                      {
                        "message": [
                          {
                            "context": "var x: ?boolean = true;",
                            "descr": "Potentially null/undefined value.",
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
                          },
                          {
                            "context": "var x: ?boolean = true;",
                            "descr": "Potentially false value.",
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
                      }
                    ],
                    "kind": "lint",
                    "level": "warning",
                    "suppressions": [],
                    "message": [
                      {
                        "context": "if (x) {",
                        "descr": "sketchy-null-bool: Sketchy null check on boolean value. Perhaps you meant to check for null instead of for existence?",
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
      .ideNewMessagesWithTimeout(
        5000,
        [
          {
            "method": "diagnosticsNotification",
            "params": [
              {
                "flowVersion": "<VERSION STUBBED FOR TEST>",
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      )
      .because('When we close a new file we get the current errors'),

    ideNotification('didClose', 'fileWithWarning.js')
      .ideNoNewMessagesAfterSleep(500)
      .because(
        'When we close an already closed file, we dont get the current errors',
      ),
  ]),

  test('Stop the flow ide command without killing the server', [
    ideStart(),
    addCode('var x = 123')
      .ideStop()
      .sleep(500),
    addCode('var y: string = 123')
      .newErrors(
        `
          test.js:5
            5: var y: string = 123
                               ^^^ number. This type is incompatible with
            5: var y: string = 123
                      ^^^^^^ string
        `,
      )
      .because('Stopping the flow ide command used to kill the server accidentally'),
  ]),
]);
