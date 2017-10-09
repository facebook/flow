/*
 * @flow
 * @lint-ignore-every LINE_WRAP1
 */


import {suite, test} from '../../../tsrc/test/Tester';

export default suite(({
  addCode, addFile, addFiles, removeFile, ideStart, ideNotification, flowCmd
}) => [
  test('Opening and closing single file with no dependents or dependencies', [
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
      ),

    addCode('var x: string = 123')
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
                        "context": "var x: string = 123",
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
                        "context": "var x: string = 123",
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
                        "context": "var x: string = 123",
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
                        "context": "var x: string = 123",
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
      .because('Opening the file triggers a recheck which sees the error'),

    ideNotification('didClose', 'test.js')
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
                        "context": "var x: string = 123",
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
                        "context": "var x: string = 123",
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
      .because('Closing the file does not trigger recheck, just send errors'),

    flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error: test.js:3
            3: var x: string = 123
                               ^^^ number. This type is incompatible with
            3: var x: string = 123
                      ^^^^^^ string


          Found 1 error

        `,
      )
      .because('Even though test.js is closed, it is still focused'),

    addCode('var anotherError: bool = 123')
      .newErrors(
        `
          test.js:5
            5: var anotherError: bool = 123
                                        ^^^ number. This type is incompatible with
            5: var anotherError: bool = 123
                                 ^^^^ boolean
        `,
      )
      .because('Changes are still noticed'),
  ]).lazy('ide'),

  // Somewhat similar to tests/quick-start-add-dependency
  test('New dependent', [
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
      ),
    addFiles('focused.js', 'dependency.js', 'otherDependent.js')
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
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      )
      .because('Nothing is open, so we just get the recheck start and end'),
    ideNotification('didOpen', 'focused.js')
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
                        "context": "var dependencyError: string = 123;",
                        "descr": "number",
                        "type": "Blame",
                        "loc": {
                          "source": "dependency.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 31,
                            "offset": 51
                          },
                          "end": {
                            "line": 3,
                            "column": 33,
                            "offset": 54
                          }
                        },
                        "path": "dependency.js",
                        "line": 3,
                        "endline": 3,
                        "start": 31,
                        "end": 33
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
                        "context": "var dependencyError: string = 123;",
                        "descr": "string",
                        "type": "Blame",
                        "loc": {
                          "source": "dependency.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 22,
                            "offset": 42
                          },
                          "end": {
                            "line": 3,
                            "column": 27,
                            "offset": 48
                          }
                        },
                        "path": "dependency.js",
                        "line": 3,
                        "endline": 3,
                        "start": 22,
                        "end": 27
                      }
                    ]
                  }
                ],
                "passed": false
              }
            ]
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
                        "context": "var focusedError: string = 123;",
                        "descr": "number",
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
                            "column": 30,
                            "offset": 70
                          }
                        },
                        "path": "focused.js",
                        "line": 3,
                        "endline": 3,
                        "start": 28,
                        "end": 30
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
                        "context": "var focusedError: string = 123;",
                        "descr": "string",
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
                        "context": "var dependencyError: string = 123;",
                        "descr": "number",
                        "type": "Blame",
                        "loc": {
                          "source": "dependency.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 31,
                            "offset": 51
                          },
                          "end": {
                            "line": 3,
                            "column": 33,
                            "offset": 54
                          }
                        },
                        "path": "dependency.js",
                        "line": 3,
                        "endline": 3,
                        "start": 31,
                        "end": 33
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
                        "context": "var dependencyError: string = 123;",
                        "descr": "string",
                        "type": "Blame",
                        "loc": {
                          "source": "dependency.js",
                          "type": "SourceFile",
                          "start": {
                            "line": 3,
                            "column": 22,
                            "offset": 42
                          },
                          "end": {
                            "line": 3,
                            "column": 27,
                            "offset": 48
                          }
                        },
                        "path": "dependency.js",
                        "line": 3,
                        "endline": 3,
                        "start": 22,
                        "end": 27
                      }
                    ]
                  },
                  {
                    "kind": "infer",
                    "level": "error",
                    "suppressions": [],
                    "message": [
                      {
                        "context": "var focusedError: string = 123;",
                        "descr": "number",
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
                            "column": 30,
                            "offset": 70
                          }
                        },
                        "path": "focused.js",
                        "line": 3,
                        "endline": 3,
                        "start": 28,
                        "end": 30
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
                        "context": "var focusedError: string = 123;",
                        "descr": "string",
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
                "passed": false
              }
            ]
          }
        ],
      )
      .because(
        'Opening focused.js will cause a recheck and show the errors in focused.js and dependency.js'
      ),
    flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error: dependency.js:3
            3: var dependencyError: string = 123;
                                             ^^^ number. This type is incompatible with
            3: var dependencyError: string = 123;
                                    ^^^^^^ string

          Error: focused.js:3
            3: var focusedError: string = 123;
                                          ^^^ number. This type is incompatible with
            3: var focusedError: string = 123;
                                 ^^^^^^ string


          Found 2 errors

        `,
      ).because('There are two errors now'),

    addFile('dependent.js')
      .newErrors(
        `
          dependent.js:3
            3: var dependentError: string = 123;
                                            ^^^ number. This type is incompatible with
            3: var dependentError: string = 123;
                                   ^^^^^^ string
        `,
      )
      .because('Adding a new dependency will correctly recheck that dependency'),

  ]).lazy('ide'),

  // Based on tests/quick-start
  test('@flow and @noflow pragmas', [
    ideStart()
      .ideNotification('subscribeToDiagnostics')
      .ideNotification('didOpen', 'errors.js')
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
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      ),
    addFile('errorsWithFlowPragma.js', 'errors.js')
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
                        "context": "var x: string = 123;",
                        "descr": "number",
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
                        "context": "var x: string = 123;",
                        "descr": "number",
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
                "passed": false
              }
            ]
          },
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
                        "context": "var x: string = 123;",
                        "descr": "number",
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
                        "context": "var x: string = 123;",
                        "descr": "number",
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
                               ^^^ number. This type is incompatible with
            2: var x: string = 123;
                      ^^^^^^ string
        `,
      )
      .because('File is open and has @flow so we should get the error'),
    addFile('errorsWithNoFlowPragma.js', 'errors.js')
      .flowCmd(['status', '--strip-root'])
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

        `,
      )
      .because('File is open but has @noflow so we should no errors'),
    addFile('errorsWithFlowPragma.js', 'errors.js')
      .newErrors(
        `
          errors.js:2
            2: var x: string = 123;
                               ^^^ number. This type is incompatible with
            2: var x: string = 123;
                      ^^^^^^ string
        `,
      )
      .because('Reverting back to @flow should show the error again'),
  ]).lazy('ide').flowConfig('_flowconfig_all_false'),

  // Based on tests/quick-start-add-dependency-on-cycle
  test('Open file cyclic dependency', [
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
      ),
    addFiles('cycleA.js', 'cycleB.js', 'focusedWithCyclicDependency.js')
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
                "errors": [],
                "passed": true
              }
            ]
          }
        ],
      ),
    ideNotification('didOpen', 'focusedWithCyclicDependency.js')
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
                        "context": "var x: string = 123;",
                        "descr": "number",
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
                        "context": "var x: string = 123;",
                        "descr": "number",
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
                "passed": false
              }
            ]
          }
        ],
      ),
      flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error: focusedWithCyclicDependency.js:3
            3: var x: string = 123;
                               ^^^ number. This type is incompatible with
            3: var x: string = 123;
                      ^^^^^^ string


          Found 1 error

        `,
      )
      .because('Nothing should blow up and we should see the single error'),
  ]).lazy('ide'),

  // Based on tests/quick-start-check-contents
  test('check-contents should pull in dependency', [
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
      ),
    addFiles('focused.js', 'dependency.js', 'otherDependent.js')
      .flowCmd(['status', '--strip-root'])
      .stdout(
        `
          No errors!

        `,
      )
      .because('No files are open yet'),
    flowCmd(['check-contents', 'focused.js'], 'focused.js')
      .stdout(
        `
          Error: focused.js:3
            3: var focusedError: string = 123;
                                          ^^^ number. This type is incompatible with
            3: var focusedError: string = 123;
                                 ^^^^^^ string


          Found 1 error

        `,
      )
      .because('check-contents will report the error in the file it checks'),
    flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error: dependency.js:3
            3: var dependencyError: string = 123;
                                             ^^^ number. This type is incompatible with
            3: var dependencyError: string = 123;
                                    ^^^^^^ string


          Found 1 error

        `,
      )
      .because(
        'As a side effect of check-contents, dependency.js is added to the checkset. At the moment we do not prune the checked set.'
      ),
  ]).lazy('ide'),

  // Based on tests/quick-start-delete-dependency
  test('Delete dependency', [
    ideStart()
      .ideNotification('subscribeToDiagnostics')
      .ideNotification('didOpen', 'focused.js')
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
          Error: dependency.js:3
            3: var dependencyError: string = 123;
                                             ^^^ number. This type is incompatible with
            3: var dependencyError: string = 123;
                                    ^^^^^^ string

          Error: focused.js:3
            3: var focusedError: string = 123;
                                          ^^^ number. This type is incompatible with
            3: var focusedError: string = 123;
                                 ^^^^^^ string


          Found 2 errors

        `,
      )
      .because('Two errors: one in each file'),
    removeFile('dependency.js')
      .flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error: focused.js:1
            1: import dependency from './dependency';
                                      ^^^^^^^^^^^^^^ ./dependency. Required module not found

          Error: focused.js:3
            3: var focusedError: string = 123;
                                          ^^^ number. This type is incompatible with
            3: var focusedError: string = 123;
                                 ^^^^^^ string


          Found 2 errors

        `,
      )
      .because('Error in dependency.js disappears and we get missing module error'),
    addFile('dependency.js')
    .flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error: dependency.js:3
            3: var dependencyError: string = 123;
                                             ^^^ number. This type is incompatible with
            3: var dependencyError: string = 123;
                                    ^^^^^^ string

          Error: focused.js:3
            3: var focusedError: string = 123;
                                          ^^^ number. This type is incompatible with
            3: var focusedError: string = 123;
                                 ^^^^^^ string


          Found 2 errors

        `,
      )
      .because('Restoring the file takes us back to the original two errors'),
  ]).lazy('ide'),

  test('Open a dependency', [
    ideStart()
      .ideNotification('subscribeToDiagnostics')
      .ideNotification('didOpen', 'focused.js')
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
          dependency.js:3
            3: var dependencyError: string = 123;
                                             ^^^ number. This type is incompatible with
            3: var dependencyError: string = 123;
                                    ^^^^^^ string

          focused.js:3
            3: var focusedError: string = 123;
                                          ^^^ number. This type is incompatible with
            3: var focusedError: string = 123;
                                 ^^^^^^ string
        `,
      )
      .because('Other dependent is a dependent of a dependency, so is not checked'),
    ideNotification('didOpen', 'dependency.js')
      // Unfortunately ideNewMessagesWithTimeout doesn't work here since the
      // order of the streamed errors isn't fixed :(
      .sleep(500),
    flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error: dependency.js:3
            3: var dependencyError: string = 123;
                                             ^^^ number. This type is incompatible with
            3: var dependencyError: string = 123;
                                    ^^^^^^ string

          Error: focused.js:3
            3: var focusedError: string = 123;
                                          ^^^ number. This type is incompatible with
            3: var focusedError: string = 123;
                                 ^^^^^^ string

          Error: otherDependent.js:3
            3: var otherDependentError: string = 123;
                                                 ^^^ number. This type is incompatible with
            3: var otherDependentError: string = 123;
                                        ^^^^^^ string


          Found 3 errors

        `,
      )
      .because('Focusing on a dependency now checks its dependents'),
  ]).lazy('ide'),

  test('Remove and restore an open file', [
    ideStart()
      .ideNotification('subscribeToDiagnostics')
      .ideNotification('didOpen', 'focused.js')
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
          Error: dependency.js:3
            3: var dependencyError: string = 123;
                                             ^^^ number. This type is incompatible with
            3: var dependencyError: string = 123;
                                    ^^^^^^ string

          Error: focused.js:3
            3: var focusedError: string = 123;
                                          ^^^ number. This type is incompatible with
            3: var focusedError: string = 123;
                                 ^^^^^^ string


          Found 2 errors

        `,
      ),
    removeFile('focused.js')
      .addFile('focused.js')
      .flowCmd(['status', '--strip-root'])
      .stdout(
        `
          Error: dependency.js:3
            3: var dependencyError: string = 123;
                                             ^^^ number. This type is incompatible with
            3: var dependencyError: string = 123;
                                    ^^^^^^ string

          Error: focused.js:3
            3: var focusedError: string = 123;
                                          ^^^ number. This type is incompatible with
            3: var focusedError: string = 123;
                                 ^^^^^^ string


          Found 2 errors

        `,
      )
      .because('We should be back at our starting state'),
  ]).lazy('ide'),

  test('flow force-recheck --focus', [
    ideStart()
      .ideNotification('subscribeToDiagnostics')
      .ideNotification('didOpen', 'focused.js')
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
          Error: dependency.js:3
            3: var dependencyError: string = 123;
                                             ^^^ number. This type is incompatible with
            3: var dependencyError: string = 123;
                                    ^^^^^^ string

          Error: focused.js:3
            3: var focusedError: string = 123;
                                          ^^^ number. This type is incompatible with
            3: var focusedError: string = 123;
                                 ^^^^^^ string


          Found 2 errors

        `,
      ).because("otherDependent's errors are ignored due to lazy mode"),
    flowCmd(['force-recheck', '--focus', 'dependency.js'])
      .flowCmd(['status', '--strip-root'])
      .stdout(
        `

          Error: dependency.js:3
            3: var dependencyError: string = 123;
                                             ^^^ number. This type is incompatible with
            3: var dependencyError: string = 123;
                                    ^^^^^^ string

          Error: focused.js:3
            3: var focusedError: string = 123;
                                          ^^^ number. This type is incompatible with
            3: var focusedError: string = 123;
                                 ^^^^^^ string

          Error: otherDependent.js:3
            3: var otherDependentError: string = 123;
                                                 ^^^ number. This type is incompatible with
            3: var otherDependentError: string = 123;
                                        ^^^^^^ string


          Found 3 errors

        `,
      ).because('force-recheck --focus promotes dependency to focused, so we see the error in otherDependent'),
  ]).lazy('ide'),
]);
