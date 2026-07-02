/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const flowJSPath = process.argv[2];
// Exercise flow.js without Node's process global, matching how the website loads it.
global.process = undefined;
const flow = require(flowJSPath);

async function main() {
  if (flow.ready != null) {
    await flow.ready;
  }

  const libFile = 'lib.js';
  flow.registerFile(libFile, `declare var MyGlobal: string;`);
  flow.initBuiltins([libFile]);

  const config = {
    'react.runtime': 'classic',
  };

  // Regression test for invalid configSchema literal
  try {
    JSON.parse(flow.configSchema);
  } catch (e) {
    throw 'configSchema literal is not valid JSON: ' + e?.message;
  }

  // Regression test for https://github.com/facebook/flow/issues/9024
  if (
    JSON.stringify(
      flow.parse('#!/usr/bin/env node\n', {tokens: true}).tokens,
    ) !==
    JSON.stringify([
      {
        type: 'T_INTERPRETER',
        context: 'normal',
        loc: {start: {line: 1, column: 0}, end: {line: 1, column: 19}},
        range: [0, 19],
        value: '/usr/bin/env node',
      },
    ])
  ) {
    throw 'Incorrect parse result';
  }

  if (flow.checkContent('test.js', 'MyGlobal;', config).length > 0) {
    throw 'There should be no errors if the library is correctly registered.';
  }
  const ambiguousObjectTypeDefault = flow.checkContent(
    'test.js',
    'type T = {foo: string};',
    config,
  );
  if (ambiguousObjectTypeDefault.length > 0) {
    throw (
      'Disabled ambiguous-object-type lint should not be reported. Got: ' +
      JSON.stringify(ambiguousObjectTypeDefault, undefined, 2)
    );
  }
  const ambiguousObjectTypeWarning = flow.checkContent(
    'test.js',
    `// flowlint ambiguous-object-type:warn
type T = {foo: string};`,
    config,
  );
  if (
    ambiguousObjectTypeWarning.length !== 1 ||
    ambiguousObjectTypeWarning[0].level !== 'warning' ||
    !JSON.stringify(ambiguousObjectTypeWarning).includes(
      'ambiguous-object-type',
    )
  ) {
    throw (
      'ambiguous-object-type should be preserved as a warning. Got: ' +
      JSON.stringify(ambiguousObjectTypeWarning, undefined, 2)
    );
  }
  if (
    flow.checkContent('test.js', 'MyGloba;', config)[0].message[0].descr !==
    'Cannot resolve name `MyGloba`. [cannot-resolve-name]'
  ) {
    throw 'Referring to non-existent global should be an error.';
  }
  if (
    flow.checkContent(
      'test.js',
      `// @jsx Foo
const Bar = '123';
function Foo(x: string) {}
<Bar />; // ok`,
      config,
    ).length > 0
  ) {
    throw 'There should be no errors if jsx pragma is correctly parsed.';
  }

  if (
    JSON.stringify(
      flow.getDef('test.js', 'const foo = 1;\nfoo', 2, 1, config),
      undefined,
      2,
    ) !==
    `[
  {
    "source": "test.js",
    "type": "SourceFile",
    "start": {
      "line": 1,
      "column": 7
    },
    "end": {
      "line": 1,
      "column": 9
    }
  }
]`
  ) {
    throw 'Incorrect get-def result';
  }
  const got = flow.autocomplete(
    'test.js',
    'const foo = 1;\nconst bar = "foo";\nfo',
    3,
    2,
    config,
  );
  if (
    JSON.stringify(got, undefined, 2) !==
    `{
  "incomplete": false,
  "suggestions": [
    {
      "additionalTextEdits": [],
      "insertText": "foo",
      "range": {
        "insert": {
          "startLineNumber": 3,
          "startColumn": 1,
          "endLineNumber": 3,
          "endColumn": 3
        },
        "replace": {
          "startLineNumber": 3,
          "startColumn": 1,
          "endLineNumber": 3,
          "endColumn": 2
        }
      },
      "detail": "1",
      "kind": 12,
      "label": "foo"
    },
    {
      "additionalTextEdits": [],
      "insertText": "for",
      "range": {
        "insert": {
          "startLineNumber": 3,
          "startColumn": 1,
          "endLineNumber": 3,
          "endColumn": 3
        },
        "replace": {
          "startLineNumber": 3,
          "startColumn": 1,
          "endLineNumber": 3,
          "endColumn": 2
        }
      },
      "kind": 14,
      "label": "for"
    },
    {
      "additionalTextEdits": [],
      "insertText": "function",
      "range": {
        "insert": {
          "startLineNumber": 3,
          "startColumn": 1,
          "endLineNumber": 3,
          "endColumn": 3
        },
        "replace": {
          "startLineNumber": 3,
          "startColumn": 1,
          "endLineNumber": 3,
          "endColumn": 2
        }
      },
      "kind": 14,
      "label": "function"
    }
  ]
}`
  ) {
    throw (
      'Invalid autocomplete result. Got: ' + JSON.stringify(got, undefined, 2)
    );
  }
}

main().catch(error => {
  setTimeout(() => {
    throw error;
  }, 0);
});
