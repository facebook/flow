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
      ).because('No errors should be streamed during the recheck'),
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
                "errors": [],
                "passed": true
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
    ideStart(),
    ideRequest('autocomplete', 'test.js', 1, 12, "({x: 123}).;")
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
]);
